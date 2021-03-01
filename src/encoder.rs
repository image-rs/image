use borrow::Cow;
use hash::Hasher;
use io::{Read, Write};
use ops::{Deref, DerefMut};
use std::{borrow, error, fmt, hash, io, mem, ops, result};

use crc32fast::Hasher as Crc32;
use deflate::write::ZlibEncoder;

use crate::chunk::{self, ChunkType};
use crate::common::{
    AnimationControl, BitDepth, BlendOp, BytesPerPixel, ColorType, Compression, DisposeOp,
    FrameControl, Info, ParameterError, ParameterErrorKind, ScaledFloat,
};
use crate::filter::{filter, AdaptiveFilterType, FilterType};
use crate::traits::WriteBytesExt;

pub type Result<T> = result::Result<T, EncodingError>;

#[derive(Debug)]
pub enum EncodingError {
    IoError(io::Error),
    Format(FormatError),
    Parameter(ParameterError),
    LimitsExceeded,
}

#[derive(Debug)]
pub struct FormatError {
    inner: FormatErrorKind,
}

#[derive(Debug)]
enum FormatErrorKind {
    ZeroWidth,
    ZeroHeight,
    InvalidColorCombination(BitDepth, ColorType),
    NoPalette,
    // TODO: wait, what?
    WrittenTooMuch(usize),
    NotAnimated,
    OutOfBounds,
    EndReached,
    ZeroFrames,
}

impl error::Error for EncodingError {
    fn cause(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            EncodingError::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl fmt::Display for EncodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        use self::EncodingError::*;
        match self {
            IoError(err) => write!(fmt, "{}", err),
            Format(desc) => write!(fmt, "{}", desc),
            Parameter(desc) => write!(fmt, "{}", desc),
            LimitsExceeded => write!(fmt, "Limits are exceeded."),
        }
    }
}

impl fmt::Display for FormatError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        use FormatErrorKind::*;
        match self.inner {
            ZeroWidth => write!(fmt, "Zero width not allowed"),
            ZeroHeight => write!(fmt, "Zero height not allowed"),
            ZeroFrames => write!(fmt, "Zero frames not allowed"),
            InvalidColorCombination(depth, color) => write!(
                fmt,
                "Invalid combination of bit-depth '{:?}' and color-type '{:?}'",
                depth, color
            ),
            NoPalette => write!(fmt, "can't write indexed image without palette"),
            WrittenTooMuch(index) => write!(fmt, "wrong data size, got {} bytes too many", index),
            NotAnimated => write!(fmt, "not an animation"),
            OutOfBounds => write!(
                fmt,
                "the dimension and position go over the frame boundaries"
            ),
            EndReached => write!(fmt, "all the frames have been already written"),
        }
    }
}

impl From<io::Error> for EncodingError {
    fn from(err: io::Error) -> EncodingError {
        EncodingError::IoError(err)
    }
}

impl From<EncodingError> for io::Error {
    fn from(err: EncodingError) -> io::Error {
        io::Error::new(io::ErrorKind::Other, err.to_string())
    }
}

// Private impl.
impl From<FormatErrorKind> for FormatError {
    fn from(kind: FormatErrorKind) -> Self {
        FormatError { inner: kind }
    }
}

/// PNG Encoder
pub struct Encoder<'a, W: Write> {
    w: W,
    info: Info<'a>,
    filter: FilterType,
    adaptive_filter: AdaptiveFilterType,
    sep_def_img: bool,
}

impl<'a, W: Write> Encoder<'a, W> {
    pub fn new(w: W, width: u32, height: u32) -> Encoder<'static, W> {
        let info = Info {
            width,
            height,
            ..Default::default()
        };
        Encoder {
            w,
            info,
            filter: FilterType::default(),
            adaptive_filter: AdaptiveFilterType::default(),
            sep_def_img: false,
        }
    }

    /// Specify that the image is animated.
    ///
    /// `num_frames` controls how many frames the animation has, while
    /// `num_plays` controls how many times the animation should be
    /// repeaded until it stops, if it's zero then it will repeat
    /// inifinitely
    ///
    /// This method returns an error if `num_frames` is 0.
    pub fn set_animated(&mut self, num_frames: u32, num_plays: u32) -> Result<()> {
        if num_frames == 0 {
            return Err(EncodingError::Format(FormatErrorKind::ZeroFrames.into()));
        }
        let actl = AnimationControl {
            num_frames,
            num_plays,
        };
        let fctl = FrameControl {
            sequence_number: 0,
            width: self.info.width,
            height: self.info.height,
            ..Default::default()
        };
        self.info.animation_control = Some(actl);
        self.info.frame_control = Some(fctl);
        Ok(())
    }

    pub fn set_sep_def_img(&mut self, sep_def_img: bool) -> Result<()> {
        if self.info.animation_control.is_none() {
            self.sep_def_img = sep_def_img;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_palette(&mut self, palette: Cow<'a, [u8]>) {
        self.info.palette = Some(palette);
    }

    pub fn set_trns(&mut self, trns: Cow<'a, [u8]>) {
        self.info.trns = Some(trns);
    }

    /// Set the display gamma of the source system on which the image was generated or last edited.
    pub fn set_source_gamma(&mut self, source_gamma: ScaledFloat) {
        self.info.source_gamma = Some(source_gamma);
    }

    /// Set the chromaticities for the source system's display channels (red, green, blue) and the whitepoint
    /// of the source system on which the image was generated or last edited.
    pub fn set_source_chromaticities(
        &mut self,
        source_chromaticities: super::SourceChromaticities,
    ) {
        self.info.source_chromaticities = Some(source_chromaticities);
    }

    /// Mark the image data as conforming to the SRGB color space with the specified rendering intent.
    ///
    /// Matching source gamma and chromaticities chunks are added automatically.
    /// Any manually specified source gamma or chromaticities will be ignored.
    pub fn set_srgb(&mut self, rendering_intent: super::SrgbRenderingIntent) {
        self.info.srgb = Some(rendering_intent);
    }

    pub fn write_header(self) -> Result<Writer<W>> {
        Writer::new(
            self.w,
            PartialInfo::new(&self.info),
            self.filter,
            self.adaptive_filter,
            self.sep_def_img,
        )
        .init(&self.info)
    }

    /// Set the color of the encoded image.
    ///
    /// These correspond to the color types in the png IHDR data that will be written. The length
    /// of the image data that is later supplied must match the color type, otherwise an error will
    /// be emitted.
    pub fn set_color(&mut self, color: ColorType) {
        self.info.color_type = color;
    }

    /// Set the indicated depth of the image data.
    pub fn set_depth(&mut self, depth: BitDepth) {
        self.info.bit_depth = depth;
    }

    /// Set compression parameters.
    ///
    /// Accepts a `Compression` or any type that can transform into a `Compression`. Notably `deflate::Compression` and
    /// `deflate::CompressionOptions` which "just work".
    pub fn set_compression(&mut self, compression: Compression) {
        self.info.compression = compression;
    }

    /// Set the used filter type.
    ///
    /// The default filter is [`FilterType::Sub`] which provides a basic prediction algorithm for
    /// sample values based on the previous. For a potentially better compression ratio, at the
    /// cost of more complex processing, try out [`FilterType::Paeth`].
    ///
    /// [`FilterType::Sub`]: enum.FilterType.html#variant.Sub
    /// [`FilterType::Paeth`]: enum.FilterType.html#variant.Paeth
    pub fn set_filter(&mut self, filter: FilterType) {
        self.filter = filter;
    }

    /// Set the adaptive filter type.
    ///
    /// Adaptive filtering attempts to select the best filter for each line
    /// based on heuristics which minimize the file size for compression rather
    /// than use a single filter for the entire image. The default method is
    /// [`AdaptiveFilterType::NonAdaptive`].
    ///
    /// [`AdaptiveFilterType::NonAdaptive`]: enum.AdaptiveFilterType.html
    pub fn set_adaptive_filter(&mut self, adaptive_filter: AdaptiveFilterType) {
        self.adaptive_filter = adaptive_filter;
    }

    /// Set the fraction of time every frame is going to be displayed, in seconds.
    ///
    /// *Note that this parameter can be set for each individual frame after
    /// [`write_header`] is called. (see [`Writer::set_frame_delay`])*
    ///
    /// If the denominator is 0, it is to be treated as if it were 100
    /// (that is, the numerator then specifies 1/100ths of a second).
    /// If the the value of the numerator is 0 the decoder should render the next frame
    /// as quickly as possible, though viewers may impose a reasonable lower bound.
    ///
    /// The default value is 0 for both the numerator and denominator.
    ///
    /// This method will return an error if the image is not animated.
    /// (see [`set_animated`])
    ///
    /// [`write_header`]: struct.Encoder.html#method.write_header
    /// [`set_animated`]: struct.Encoder.html#method.set_animated
    /// [`Writer::set_frame_delay`]: struct.Writer#method.set_frame_delay
    pub fn set_frame_delay(&mut self, numerator: u16, denominator: u16) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.delay_den = denominator;
            fctl.delay_num = numerator;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the blend operation for every frame.
    ///
    /// The blend operation specifies whether the frame is to be alpha blended
    /// into the current output buffer content, or whether it should completely
    /// replace its region in the output buffer.
    ///
    /// *Note that this parameter can be set for each individual frame after
    /// [`writer_header`] is called. (see [`Writer::set_blend_op`])*
    ///
    /// See the [`BlendOp`] documentaion for the possible values and their effects.
    ///
    /// *Note that for the first frame the two blend modes are functionally
    /// equivalent due to the clearing of the output buffer at the beginning
    /// of each play.*
    ///
    /// The default value is [`BlendOp::Source`].
    ///
    /// This method will return an error if the image is not animated.
    /// (see [`set_animated`])
    ///
    /// [`BlendOP`]: enum.BlendOp.html
    /// [`BlendOP::Source`]: enum.BlendOp.html#variant.Source
    /// [`write_header`]: struct.Encoder.html#method.write_header
    /// [`set_animated`]: struct.Encoder.html#method.set_animated
    /// [`Writer::set_blend_op`]: struct.Writer#method.set_blend_op
    pub fn set_blend_op(&mut self, op: BlendOp) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.blend_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the dispose operation for every frame.
    ///
    /// The dispose operation specifies how the output buffer should be changed
    /// at the end of the delay (before rendering the next frame)
    ///
    /// *Note that this parameter can be set for each individual frame after
    /// [`writer_header`] is called (see [`Writer::set_dispose_op`])*
    ///
    /// See the [`DisposeOp`] documentaion for the possible values and their effects.
    ///
    /// *Note that if the first frame uses [`DisposeOp::Previous`]
    /// it will be treated as [`DisposeOp::Background`].*
    ///
    /// The default value is [`DisposeOp::None`].
    ///
    /// This method will return an error if the image is not animated.
    /// (see [`set_animated`])
    ///
    /// [`DisposeOp`]: ../common/enum.BlendOp.html
    /// [`DisposeOp::Previous`]: ../common/enum.BlendOp.html#variant.Previous
    /// [`DisposeOp::Background`]: ../common/enum.BlendOp.html#variant.Background
    /// [`DisposeOp::None`]: ../common/enum.BlendOp.html#variant.None
    /// [`write_header`]: struct.Encoder.html#method.write_header
    /// [`set_animated`]: struct.Encoder.html#method.set_animated
    /// [`Writer::set_dispose_op`]: struct.Writer#method.set_dispose_op
    pub fn set_dispose_op(&mut self, op: DisposeOp) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.dispose_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }
}

/// PNG writer
pub struct Writer<W: Write> {
    w: W,
    info: PartialInfo,
    filter: FilterType,
    adaptive_filter: AdaptiveFilterType,
    sep_def_img: bool,
    written: u64,
}

/// Contains the subset of attributes of [Info] needed for [Writer] to function
struct PartialInfo {
    width: u32,
    height: u32,
    bit_depth: BitDepth,
    color_type: ColorType,
    frame_control: Option<FrameControl>,
    animation_control: Option<AnimationControl>,
    compression: Compression,
    has_palette: bool,
}

impl PartialInfo {
    fn new(info: &Info) -> Self {
        PartialInfo {
            width: info.width,
            height: info.height,
            bit_depth: info.bit_depth,
            color_type: info.color_type,
            frame_control: info.frame_control,
            animation_control: info.animation_control,
            compression: info.compression,
            has_palette: info.palette.is_some(),
        }
    }

    fn bpp_in_prediction(&self) -> BytesPerPixel {
        // Passthrough
        self.to_info().bpp_in_prediction()
    }

    fn raw_row_length(&self) -> usize {
        // Passthrough
        self.to_info().raw_row_length()
    }

    fn raw_row_length_from_width(&self, width: u32) -> usize {
        // Passthrough
        self.to_info().raw_row_length_from_width(width)
    }

    /// Converts this partial info to an owned Info struct,
    /// setting missing values to their defaults
    fn to_info(&self) -> Info<'static> {
        Info {
            width: self.width,
            height: self.height,
            bit_depth: self.bit_depth,
            color_type: self.color_type,
            frame_control: self.frame_control,
            animation_control: self.animation_control,
            compression: self.compression,
            ..Default::default()
        }
    }
}

const DEFAULT_BUFFER_LENGTH: usize = 4 * 1024;

pub(crate) fn write_chunk<W: Write>(mut w: W, name: chunk::ChunkType, data: &[u8]) -> Result<()> {
    w.write_be(data.len() as u32)?;
    w.write_all(&name.0)?;
    w.write_all(data)?;
    let mut crc = Crc32::new();
    crc.update(&name.0);
    crc.update(data);
    w.write_be(crc.finalize())?;
    Ok(())
}

impl<W: Write> Writer<W> {
    fn new(
        w: W,
        info: PartialInfo,
        filter: FilterType,
        adaptive_filter: AdaptiveFilterType,
        sep_def_img: bool,
    ) -> Writer<W> {
        Writer {
            w,
            info,
            filter,
            adaptive_filter,
            sep_def_img,
            written: 0,
        }
    }

    fn init(mut self, info: &Info<'_>) -> Result<Self> {
        if self.info.width == 0 {
            return Err(EncodingError::Format(FormatErrorKind::ZeroWidth.into()));
        }

        if self.info.height == 0 {
            return Err(EncodingError::Format(FormatErrorKind::ZeroHeight.into()));
        }

        if self
            .info
            .color_type
            .is_combination_invalid(self.info.bit_depth)
        {
            return Err(EncodingError::Format(
                FormatErrorKind::InvalidColorCombination(self.info.bit_depth, self.info.color_type)
                    .into(),
            ));
        }

        self.w.write_all(&[137, 80, 78, 71, 13, 10, 26, 10])?; // PNG signature
        info.encode(&mut self.w)?;

        Ok(self)
    }

    pub fn write_chunk(&mut self, name: ChunkType, data: &[u8]) -> Result<()> {
        write_chunk(&mut self.w, name, data)
    }

    fn max_frames(&self) -> u64 {
        match self.info.animation_control {
            Some(a) if self.sep_def_img => a.num_frames as u64 + 1,
            Some(a) => a.num_frames as u64,
            None => 1,
        }
    }

    /// Writes the image data.
    pub fn write_image_data(&mut self, data: &[u8]) -> Result<()> {
        const MAX_IDAT_CHUNK_LEN: u32 = std::u32::MAX >> 1;
        const MAX_fdAT_CHUNK_LEN: u32 = (std::u32::MAX >> 1) - 4;

        if self.info.color_type == ColorType::Indexed && !self.info.has_palette {
            return Err(EncodingError::Format(FormatErrorKind::NoPalette.into()));
        }

        if self.written > self.max_frames() {
            return Err(EncodingError::Format(FormatErrorKind::EndReached.into()));
        }

        let width: usize;
        let height: usize;
        if let Some(ref mut fctl) = self.info.frame_control {
            width = fctl.width as usize;
            height = fctl.height as usize;
        } else {
            width = self.info.width as usize;
            height = self.info.height as usize;
        }

        let in_len = self.info.raw_row_length_from_width(width as u32) - 1;
        let data_size = in_len * height;
        if data_size != data.len() {
            return Err(EncodingError::Parameter(
                ParameterErrorKind::ImageBufferSize {
                    expected: data_size,
                    actual: data.len(),
                }
                .into(),
            ));
        }

        let prev = vec![0; in_len];
        let mut prev = prev.as_slice();
        let mut current = vec![0; in_len];

        let mut zlib = deflate::write::ZlibEncoder::new(
            Vec::new(),
            self.info.compression.clone().to_options(),
        );
        let bpp = self.info.bpp_in_prediction();
        let filter_method = self.filter;
        let adaptive_method = self.adaptive_filter;
        for line in data.chunks(in_len) {
            current.copy_from_slice(&line);
            let filter_type = filter(filter_method, adaptive_method, bpp, &prev, &mut current);
            zlib.write_all(&[filter_type as u8])?;
            zlib.write_all(&current)?;
            prev = line;
        }
        let zlib_encoded = zlib.finish()?;
        if self.sep_def_img || self.info.frame_control.is_none() {
            self.sep_def_img = false;
            for chunk in zlib_encoded.chunks(MAX_IDAT_CHUNK_LEN as usize) {
                self.write_chunk(chunk::IDAT, &chunk)?;
            }
        } else if let Some(ref mut fctl) = self.info.frame_control {
            fctl.encode(&mut self.w)?;
            fctl.sequence_number = fctl.sequence_number.wrapping_add(1);

            if self.written == 0 {
                for chunk in zlib_encoded.chunks(MAX_IDAT_CHUNK_LEN as usize) {
                    self.write_chunk(chunk::IDAT, &chunk)?;
                }
            } else {
                let buff_size = zlib_encoded.len().min(MAX_fdAT_CHUNK_LEN as usize);
                let mut alldata = vec![0u8; 4 + buff_size];
                for chunk in zlib_encoded.chunks(MAX_fdAT_CHUNK_LEN as usize) {
                    alldata[..4].copy_from_slice(&fctl.sequence_number.to_be_bytes());
                    alldata[4..][..chunk.len()].copy_from_slice(chunk);
                    write_chunk(&mut self.w, chunk::fdAT, &alldata[..4 + chunk.len()])?;
                    fctl.sequence_number = fctl.sequence_number.wrapping_add(1);
                }
            }
        } else {
            unreachable!(); // this should be unreachable
        }
        self.written += 1;
        Ok(())
    }

    /// Set the used filter type for the following frames.
    ///
    /// The default filter is [`FilterType::Sub`] which provides a basic prediction algorithm for
    /// sample values based on the previous. For a potentially better compression ratio, at the
    /// cost of more complex processing, try out [`FilterType::Paeth`].
    ///
    /// [`FilterType::Sub`]: enum.FilterType.html#variant.Sub
    /// [`FilterType::Paeth`]: enum.FilterType.html#variant.Paeth
    pub fn set_filter(&mut self, filter: FilterType) {
        self.filter = filter;
    }

    /// Set the adaptive filter type for the following frames.
    ///
    /// Adaptive filtering attempts to select the best filter for each line
    /// based on heuristics which minimize the file size for compression rather
    /// than use a single filter for the entire image. The default method is
    /// [`AdaptiveFilterType::NonAdaptive`].
    ///
    /// [`AdaptiveFilterType::NonAdaptive`]: enum.AdaptiveFilterType.html
    pub fn set_adaptive_filter(&mut self, adaptive_filter: AdaptiveFilterType) {
        self.adaptive_filter = adaptive_filter;
    }

    /// Set the fraction of time the following frames are going to be displayed,
    /// in seconds
    ///
    /// If the denominator is 0, it is to be treated as if it were 100
    /// (that is, the numerator then specifies 1/100ths of a second).
    /// If the the value of the numerator is 0 the decoder should render the next frame
    /// as quickly as possible, though viewers may impose a reasonable lower bound.
    ///
    /// This method will return an error if the image is not animated.
    pub fn set_frame_delay(&mut self, numerator: u16, denominator: u16) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.delay_den = denominator;
            fctl.delay_num = numerator;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the dimension of the following frames.
    ///
    /// This function will return an error when:
    /// - The image is not an animated;
    ///
    /// - The selected dimension, considering also the current frame position,
    ///   goes outside the image boudries;
    ///
    /// - One or both the width and height are 0;
    ///
    // ??? TODO ???
    // - The next frame is the default image
    pub fn set_frame_dimension(&mut self, width: u32, height: u32) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            if Some(width) > self.info.width.checked_sub(fctl.x_offset)
                || Some(height) > self.info.height.checked_sub(fctl.y_offset)
            {
                return Err(EncodingError::Format(FormatErrorKind::OutOfBounds.into()));
            } else if width == 0 {
                return Err(EncodingError::Format(FormatErrorKind::ZeroWidth.into()));
            } else if height == 0 {
                return Err(EncodingError::Format(FormatErrorKind::ZeroHeight.into()));
            }
            fctl.width = width;
            fctl.height = height;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the position of the following frames.
    ///
    /// An error will be returned if:
    /// - The image is not animated;
    ///
    /// - The selected position, considering also the current frame dimension,
    ///   goes outside the image boudries;
    ///
    // ??? TODO ???
    // - The next frame is the default image
    pub fn set_frame_position(&mut self, x: u32, y: u32) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            if Some(x) > self.info.width.checked_sub(fctl.width)
                || Some(y) > self.info.height.checked_sub(fctl.height)
            {
                return Err(EncodingError::Format(FormatErrorKind::OutOfBounds.into()));
            }
            fctl.x_offset = x;
            fctl.y_offset = y;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the frame dimension to occupy all the image, starting from
    /// the current position.
    ///
    /// To reset the frame to the full image size [`reset_frame_position`]
    /// should be called first.
    ///
    /// This method will return an error if the image is not animated.
    ///
    /// [`reset_frame_position`]: struct.Writer.html#method.reset_frame_position
    pub fn reset_frame_dimension(&mut self) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.width = self.info.width - fctl.x_offset;
            fctl.height = self.info.height - fctl.y_offset;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the frame position to (0, 0).
    ///
    /// Equivalent to calling [`set_frame_position(0, 0)`].
    ///
    /// This method will return an error if the image is not animated.
    ///
    /// [`set_frame_position(0, 0)`]: struct.Writer.html#method.set_frame_position
    pub fn reset_frame_position(&mut self) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.x_offset = 0;
            fctl.y_offset = 0;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the blend operation for the following frames.
    ///
    /// The blend operation specifies whether the frame is to be alpha blended
    /// into the current output buffer content, or whether it should completely
    /// replace its region in the output buffer.
    ///
    /// See the [`BlendOp`] documentaion for the possible values and their effects.
    ///
    /// *Note that for the first frame the two blend modes are functionally
    /// equivalent due to the clearing of the output buffer at the beginning
    /// of each play.*
    ///
    /// This method will return an error if the image is not animated.
    ///
    /// [`BlendOP`]: enum.BlendOp.html
    pub fn set_blend_op(&mut self, op: BlendOp) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.blend_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Set the dispose operation for the following frames.
    ///
    /// The dispose operation specifies how the output buffer should be changed
    /// at the end of the delay (before rendering the next frame)
    ///
    /// See the [`DisposeOp`] documentaion for the possible values and their effects.
    ///
    /// *Note that if the first frame uses [`DisposeOp::Previous`]
    /// it will be treated as [`DisposeOp::Background`].*
    ///
    /// This method will return an error if the image is not animated.
    ///
    /// [`DisposeOp`]: ../common/enum.BlendOp.html
    /// [`DisposeOp::Previous`]: ../common/enum.BlendOp.html#variant.Previous
    /// [`DisposeOp::Background`]: ../common/enum.BlendOp.html#variant.Background
    pub fn set_dispose_op(&mut self, op: DisposeOp) -> Result<()> {
        if let Some(ref mut fctl) = self.info.frame_control {
            fctl.dispose_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Create a stream writer.
    ///
    /// This allows you to create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chunk
    /// size.
    ///
    /// This borrows the writer which allows for manually appending additional
    /// chunks after the image data has been written.
    pub fn stream_writer(&mut self) -> Result<StreamWriter<W>> {
        self.stream_writer_with_size(DEFAULT_BUFFER_LENGTH)
    }

    /// Create a stream writer with custom buffer size.
    ///
    /// See [`stream_writer`].
    ///
    /// [`stream_writer`]: #fn.stream_writer
    pub fn stream_writer_with_size(&mut self, size: usize) -> Result<StreamWriter<W>> {
        StreamWriter::new(ChunkOutput::Borrowed(self), size)
    }

    /// Turn this into a stream writer for image data.
    ///
    /// This allows you to create images that do not fit in memory. The default
    /// chunk size is 4K, use `stream_writer_with_size` to set another chunk
    /// size.
    pub fn into_stream_writer(self) -> Result<StreamWriter<'static, W>> {
        self.into_stream_writer_with_size(DEFAULT_BUFFER_LENGTH)
    }

    /// Turn this into a stream writer with custom buffer size.
    ///
    /// See [`into_stream_writer`].
    ///
    /// [`into_stream_writer`]: #fn.into_stream_writer
    pub fn into_stream_writer_with_size(self, size: usize) -> Result<StreamWriter<'static, W>> {
        StreamWriter::new(ChunkOutput::Owned(self), size)
    }
}

impl<W: Write> Drop for Writer<W> {
    fn drop(&mut self) {
        let _ = self.write_chunk(chunk::IEND, &[]);
    }
}

enum ChunkOutput<'a, W: Write> {
    Borrowed(&'a mut Writer<W>),
    Owned(Writer<W>),
}

// opted for deref for practical reasons
impl<'a, W: Write> Deref for ChunkOutput<'a, W> {
    type Target = Writer<W>;

    fn deref(&self) -> &Self::Target {
        match self {
            ChunkOutput::Borrowed(writer) => writer,
            ChunkOutput::Owned(writer) => writer,
        }
    }
}

impl<'a, W: Write> DerefMut for ChunkOutput<'a, W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            ChunkOutput::Borrowed(writer) => writer,
            ChunkOutput::Owned(writer) => writer,
        }
    }
}

/// < INTERNAL DOCUMENTATION >
///
/// This writer is used between the actual writer and the
/// ZlibEncoder and has the job of packaging the compressed
/// data into a PNG chunk, based on the image metadata
///
/// Currently the way it works is that the specified buffer
/// will hold one chunk at the time and bufferize the incoming
/// data until `flush` is called or the maximum chunk size
/// is reached.
///
/// The maximum chunk is the smallest between the selected buffer size
/// and u32::MAX >> 1 + 12 (which is the size of each chunk header
/// and the crc).
///
/// When a chunk has to be flushed the length (that is now known)
/// and the CRC will be written at the correct locations in the chunk.
struct ChunkWriter<'a, W: Write> {
    writer: ChunkOutput<'a, W>,
    buffer: Vec<u8>,
    /// keeps track of where the last byte was written
    index: usize,
    /// keeps track of the last written byte retalive to the chunk data field
    written: u32,
    crc32: Crc32,
}

impl<'a, W: Write> ChunkWriter<'a, W> {
    fn new(writer: ChunkOutput<'a, W>, buf_len: usize) -> ChunkWriter<'a, W> {
        // currently buf_len will determine the size of each chunk
        // the len is capped to the maximum size every chunk can hold
        // (this wont ever overflow an u32)
        //
        // TODO (maybe): find a way to hold two chunks at a time if `usize`
        //               is 65 bits.
        let len_cap = (std::u32::MAX >> 1) as usize + 4 + 4 + 4;
        ChunkWriter {
            writer,
            buffer: vec![0; len_cap.min(buf_len)],
            index: 0,
            written: 0,
            crc32: Crc32::new(),
        }
    }

    /// Returns the size of each scanline for the next frame
    /// paired with the size of the whole frame
    ///
    /// This is used by the `StreamWriter` to know when the scanline ends
    /// so it can filter compress it and also to know when to start
    /// the next one
    fn next_frame_info(&self) -> (usize, usize) {
        let wrt = self.writer.deref();

        let width: usize;
        let height: usize;
        if let Some(fctl) = wrt.info.frame_control {
            width = fctl.width as usize;
            height = fctl.height as usize;
        } else {
            width = wrt.info.width as usize;
            height = wrt.info.height as usize;
        }

        let in_len = wrt.info.raw_row_length_from_width(width as u32) - 1;
        let data_size = in_len * height;

        (in_len, data_size)
    }

    /// NOTE: this bypasses the internal buffer so the flush method should be called before this
    ///       in the case there is some data left in the buffer when this is called, it will panic
    fn write_header(&mut self) -> Result<()> {
        if self.index != 0 {
            // return Err(todo!())
            panic!("Called when not flushed") // TODO: add error code
        }
        let wrt = self.writer.deref_mut();
        if !wrt.sep_def_img {
            if let Some(ref mut fctl) = wrt.info.frame_control {
                fctl.encode(&mut wrt.w)?;
                fctl.sequence_number += 1;
            }
        }
        Ok(())
    }

    /// Set the `FrameControl` for the following frame
    ///
    /// It will ignore the `sequence_number` of the parameter
    /// as it is updated internally.
    ///
    /// !!! TODO !!! It will also check that everything is correct
    fn set_fctl(&mut self, f: FrameControl) -> Result<()> {
        let PartialInfo { width, height, .. } = self.writer.info;
        if let Some(ref mut fctl) = self.writer.info.frame_control {
            if f.width + f.x_offset > width || f.height + f.y_offset > height {
                return Err(EncodingError::Format(FormatErrorKind::OutOfBounds.into()));
            }
            // ingnore the sequence number
            *fctl = FrameControl {
                sequence_number: fctl.sequence_number,
                ..f
            };
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// This method will:
    /// - compute the CRC on all the compressed data
    /// - append the CRC to the end
    /// - write the length in the chunk header
    /// - flush the internal buffer
    /// - reset the counters
    fn flush_inner(&mut self) -> io::Result<()> {
        if self.index > 0 {
            // get the value of the computed crc32
            let mut crc32 = Crc32::new();
            std::mem::swap(&mut crc32, &mut self.crc32);
            let crc32 = crc32.finalize().to_be_bytes();

            // write the length of the chunk and the crc32
            self.buffer[..4].copy_from_slice(&self.written.to_be_bytes());
            self.buffer[self.index..][..4].copy_from_slice(&crc32);

            // flush the chunk and reset everything
            self.writer.w.write_all(&self.buffer[..self.index + 4])?;
            self.index = 0;
            self.written = 0;
        }
        Ok(())
    }

    /// Returns the maximum size the data field of each chunk can be
    fn max_chunk_size(&self) -> u32 {
        // buffer length - size of length field - size of chunk name - size of crc
        (std::u32::MAX >> 1).min((self.buffer.len() - 4 - 4 - 4) as u32)
    }
}

impl<'a, W: Write> Write for ChunkWriter<'a, W> {
    fn write(&mut self, mut data: &[u8]) -> io::Result<usize> {
        if data.is_empty() {
            return Ok(0);
        }

        // index == 0 means a chunk as been flushed out
        if self.index == 0 {
            // find the correct chunk to write and leave first 4 bytes for length
            // (the length can't be know at this point)
            let wrt = self.writer.deref_mut();
            if wrt.sep_def_img || wrt.info.frame_control.is_none() || wrt.written == 0 {
                self.buffer[4..8].copy_from_slice(&chunk::IDAT.0);
                self.index = 8;
            } else {
                let fctl = wrt.info.frame_control.as_mut().unwrap();
                self.buffer[4..8].copy_from_slice(&chunk::fdAT.0);
                self.buffer[8..12].copy_from_slice(&fctl.sequence_number.to_be_bytes());
                fctl.sequence_number += 1;
                self.index = 12;
                self.written = 4;
            }
            self.crc32.write(&self.buffer[4..self.index]);
        }

        // cap the buffer length to the maximum nuber of bytes that can't still
        // be added to the current chunk
        let cap = data
            .len()
            .min((self.written + self.max_chunk_size()) as usize);
        data = &data[..cap];

        let written = data.len().min(self.buffer.len() - self.index);
        data = &data[..written];

        self.buffer[self.index..][..written].copy_from_slice(data);
        self.index += written;
        self.written += written as u32;

        // compute the crc only on the written data
        self.crc32.write(data);

        // if the maximum data for this chunk as been reached it needs to be flushed
        if self.written == self.max_chunk_size() {
            self.flush_inner()?;
        }
        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.flush_inner()
    }
}

impl<W: Write> Drop for ChunkWriter<'_, W> {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

/// Streaming PNG writer
///
/// This may silently fail in the destructor, so it is a good idea to call
/// [`finish`](#method.finish) or [`flush`] before dropping.
///
/// [`flush`]: https://doc.rust-lang.org/stable/std/io/trait.Write.html#tymethod.flush
pub struct StreamWriter<'a, W: Write> {
    /// The option here is needed in order to access the inner `ChunkWriter` in-between
    /// each frame, which is needed for writing the fcTL chunks between each frame
    writer: Option<ZlibEncoder<ChunkWriter<'a, W>>>,
    prev_buf: Vec<u8>,
    curr_buf: Vec<u8>,
    /// Amount of data already written
    index: usize,
    /// length of the current scanline
    line_len: usize,
    /// size of the frame (width * height * sample_size)
    to_write: usize,
    /// Flag used to signal the end of the image
    end: bool,

    // --- cloned parameters ---
    bpp: BytesPerPixel,
    filter: FilterType,
    adaptive_filter: AdaptiveFilterType,
    fctl: Option<FrameControl>,
    compression: Compression,
    // -------------------------
}

impl<'a, W: Write> StreamWriter<'a, W> {
    fn new(writer: ChunkOutput<'a, W>, buf_len: usize) -> Result<StreamWriter<'a, W>> {
        if writer.max_frames() < writer.written {
            return Err(EncodingError::Format(FormatErrorKind::EndReached.into()));
        }
        let bpp = writer.info.bpp_in_prediction();
        let in_len = writer.info.raw_row_length() - 1;
        let filter = writer.filter;
        let adaptive_filter = writer.adaptive_filter;
        let prev_buf = vec![0; in_len];
        let curr_buf = vec![0; in_len];
        let fctl = writer.info.frame_control;

        let compression = writer.info.compression;
        let mut chunk_writer = ChunkWriter::new(writer, buf_len);
        let (line_len, to_write) = chunk_writer.next_frame_info();
        chunk_writer.write_header()?;
        let zlib = ZlibEncoder::new(chunk_writer, compression.clone().to_options());

        Ok(StreamWriter {
            writer: Some(zlib),
            index: 0,
            prev_buf,
            curr_buf,
            end: false,
            bpp,
            filter,
            adaptive_filter,
            line_len,
            to_write,
            fctl,
            compression,
        })
    }

    /// Set the used filter type for the next frame.
    ///
    /// The default filter is [`FilterType::Sub`] which provides a basic prediction algorithm for
    /// sample values based on the previous. For a potentially better compression ratio, at the
    /// cost of more complex processing, try out [`FilterType::Paeth`].
    ///
    /// [`FilterType::Sub`]: enum.FilterType.html#variant.Sub
    /// [`FilterType::Paeth`]: enum.FilterType.html#variant.Paeth
    pub fn set_filter(&mut self, filter: FilterType) {
        self.filter = filter;
    }

    /// Set the adaptive filter type for the next frame.
    ///
    /// Adaptive filtering attempts to select the best filter for each line
    /// based on heuristics which minimize the file size for compression rather
    /// than use a single filter for the entire image. The default method is
    /// [`AdaptiveFilterType::NonAdaptive`].
    ///
    /// [`AdaptiveFilterType::NonAdaptive`]: enum.AdaptiveFilterType.html
    pub fn set_adaptive_filter(&mut self, adaptive_filter: AdaptiveFilterType) {
        self.adaptive_filter = adaptive_filter;
    }

    /// Sets the dispose operation for the next frame.
    pub fn set_frame_delay(&mut self, delay_num: u16, delay_den: u16) -> Result<()> {
        if let Some(ref mut fctl) = self.fctl {
            fctl.delay_den = delay_den;
            fctl.delay_num = delay_num;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_frame_dimesion(&mut self, width: u32, height: u32) -> Result<()> {
        if let Some(ref mut fctl) = self.fctl {
            fctl.width = width;
            fctl.height = height;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn set_frame_position(&mut self, x: u32, y: u32) -> Result<()> {
        if let Some(ref mut fctl) = self.fctl {
            fctl.x_offset = x;
            fctl.y_offset = y;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    // pub fn reset_frame_dimesion(&mut self) -> Result<()> {
    //     if let Some(ref mut fctl) = self.fctl {
    //         fctl.width = self.width;
    //         fctl.height = self.height;
    //         Ok(())
    //     } else {
    //         Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
    //     }
    // }

    pub fn reset_frame_position(&mut self) -> Result<()> {
        if let Some(ref mut fctl) = self.fctl {
            fctl.x_offset = 0;
            fctl.y_offset = 0;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Sets the blend operation for the next frame.
    pub fn set_blend_op(&mut self, op: BlendOp) -> Result<()> {
        if let Some(ref mut fctl) = self.fctl {
            fctl.blend_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    /// Sets the dispose operation for the next frame.
    pub fn set_dispose_op(&mut self, op: DisposeOp) -> Result<()> {
        if let Some(ref mut fctl) = self.fctl {
            fctl.dispose_op = op;
            Ok(())
        } else {
            Err(EncodingError::Format(FormatErrorKind::NotAnimated.into()))
        }
    }

    pub fn finish(mut self) -> Result<()> {
        // TODO: call `writer.finish` somehow?
        self.flush()?;
        Ok(())
    }

    /// Flushes the buffered chunk, checks if it was the last frame,
    /// writes the next frame header and gets the next frame scanline size
    /// and image size.
    fn new_frame(
        wrt: &mut ChunkWriter<'a, W>,
        fctl: Option<FrameControl>,
    ) -> Result<(bool, usize, usize)> {
        wrt.flush()?;
        wrt.writer.written += 1;
        if let Some(fctl) = fctl {
            wrt.set_fctl(fctl)?;
        }
        wrt.write_header()?;
        let (scansize, size) = wrt.next_frame_info();
        Ok((wrt.writer.written > wrt.writer.max_frames(), scansize, size))
    }
}

impl<'a, W: Write> Write for StreamWriter<'a, W> {
    fn write(&mut self, mut data: &[u8]) -> io::Result<usize> {
        if data.is_empty() {
            return Ok(0);
        }

        if self.end {
            let err = FormatErrorKind::EndReached.into();
            return Err(EncodingError::Format(err).into());
        }

        if self.to_write == 0 {
            let mut wrt = self.writer.take().unwrap().finish()?;
            // the function is used in order to avoid the invalidation of the `StreamWriter`
            // in the case of an error occrring in this situation
            //
            // This might not work and that's because I don't know if the
            // ZlibEncoder would actually write something if no data was
            // written which would happen on the second try after the error
            // becuase `to_write` would still be at zero and `ZlibEncoder::finish`
            // would be called once again.
            //
            // A better solution might be using a custom enum for the writer that
            // can hold either a `ZlibEncoder` or a `ChunkWriter` so that when an error
            // would occur there wouldn't be the need to create another `ZlibEncoder`.
            let res = Self::new_frame(&mut wrt, self.fctl);
            self.writer = Some(ZlibEncoder::new(wrt, self.compression.clone().to_options()));
            let (end, line_len, to_write) = res?;
            self.end = end;
            self.line_len = line_len;
            self.to_write = to_write;
        }
        let written = data.read(&mut self.curr_buf[..self.line_len][self.index..])?;
        self.index += written;
        self.to_write -= written;

        if self.index == self.line_len {
            let filter_type = filter(
                self.filter,
                self.adaptive_filter,
                self.bpp,
                &self.prev_buf,
                &mut self.curr_buf,
            );
            // This can't fail as the option is used only to allow the zlib encoder to finish
            let wrt = self.writer.as_mut().unwrap();
            wrt.write_all(&[filter_type as u8])?;
            wrt.write_all(&self.curr_buf)?;
            mem::swap(&mut self.prev_buf, &mut self.curr_buf);
            self.index = 0;
        }
        Ok(written)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.writer.as_mut().unwrap().flush()?;
        if self.index > 0 {
            let err = EncodingError::Format(FormatErrorKind::WrittenTooMuch(self.index).into());
            return Err(err.into());
        }
        Ok(())
    }
}

impl<W: Write> Drop for StreamWriter<'_, W> {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Decoder;

    use rand::{thread_rng, Rng};
    use std::fs::File;
    use std::io::Write;
    use std::{cmp, io};

    #[test]
    fn roundtrip() {
        // More loops = more random testing, but also more test wait time
        for _ in 0..10 {
            for path in glob::glob("tests/pngsuite/*.png")
                .unwrap()
                .map(|r| r.unwrap())
            {
                if path.file_name().unwrap().to_str().unwrap().starts_with('x') {
                    // x* files are expected to fail to decode
                    continue;
                }
                eprintln!("{}", path.display());
                // Decode image
                let decoder = Decoder::new(File::open(path).unwrap());
                let (info, mut reader) = decoder.read_info().unwrap();
                if info.line_size != 32 {
                    // TODO encoding only works with line size 32?
                    continue;
                }
                let mut buf = vec![0; info.buffer_size()];
                eprintln!("{:?}", info);
                reader.next_frame(&mut buf).unwrap();
                // Encode decoded image
                let mut out = Vec::new();
                {
                    let mut wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut out,
                    };

                    let mut encoder = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap();
                    encoder.write_image_data(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = Decoder::new(&*out);
                let (info, mut reader) = decoder.read_info().unwrap();
                let mut buf2 = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf2).unwrap();
                // check if the encoded image is ok:
                assert_eq!(buf, buf2);
            }
        }
    }

    #[test]
    fn roundtrip_stream() {
        // More loops = more random testing, but also more test wait time
        for _ in 0..10 {
            for path in glob::glob("tests/pngsuite/*.png")
                .unwrap()
                .map(|r| r.unwrap())
            {
                if path.file_name().unwrap().to_str().unwrap().starts_with('x') {
                    // x* files are expected to fail to decode
                    continue;
                }
                // Decode image
                let decoder = Decoder::new(File::open(path).unwrap());
                let (info, mut reader) = decoder.read_info().unwrap();
                if info.line_size != 32 {
                    // TODO encoding only works with line size 32?
                    continue;
                }
                let mut buf = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf).unwrap();
                // Encode decoded image
                let mut out = Vec::new();
                {
                    let mut wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut out,
                    };

                    let mut encoder = Encoder::new(&mut wrapper, info.width, info.height)
                        .write_header()
                        .unwrap();
                    let mut stream_writer = encoder.stream_writer().unwrap();

                    let mut outer_wrapper = RandomChunkWriter {
                        rng: thread_rng(),
                        w: &mut stream_writer,
                    };

                    outer_wrapper.write_all(&buf).unwrap();
                }
                // Decode encoded decoded image
                let decoder = Decoder::new(&*out);
                let (info, mut reader) = decoder.read_info().unwrap();
                let mut buf2 = vec![0; info.buffer_size()];
                reader.next_frame(&mut buf2).unwrap();
                // check if the encoded image is ok:
                assert_eq!(buf, buf2);
            }
        }
    }

    #[test]
    fn image_palette() -> Result<()> {
        for &bit_depth in &[1u8, 2, 4, 8] {
            // Do a reference decoding, choose a fitting palette image from pngsuite
            let path = format!("tests/pngsuite/basn3p0{}.png", bit_depth);
            let decoder = Decoder::new(File::open(&path).unwrap());
            let (info, mut reader) = decoder.read_info().unwrap();

            let mut decoded_pixels = vec![0; info.buffer_size()];
            assert_eq!(
                info.width as usize * info.height as usize * usize::from(bit_depth),
                decoded_pixels.len() * 8
            );
            reader.next_frame(&mut decoded_pixels).unwrap();
            let indexed_data = decoded_pixels;

            let palette = reader.info().palette.as_ref().unwrap();
            let mut out = Vec::new();
            {
                let mut encoder = Encoder::new(&mut out, info.width, info.height);
                encoder.set_depth(BitDepth::from_u8(bit_depth).unwrap());
                encoder.set_color(ColorType::Indexed);
                encoder.set_palette(Cow::Borrowed(palette));

                let mut writer = encoder.write_header().unwrap();
                writer.write_image_data(&indexed_data).unwrap();
            }

            // Decode re-encoded image
            let decoder = Decoder::new(&*out);
            let (info, mut reader) = decoder.read_info().unwrap();
            let mut redecoded = vec![0; info.buffer_size()];
            reader.next_frame(&mut redecoded).unwrap();
            // check if the encoded image is ok:
            assert_eq!(indexed_data, redecoded);
        }
        Ok(())
    }

    #[test]
    fn expect_error_on_wrong_image_len() -> Result<()> {
        use std::io::Cursor;

        let width = 10;
        let height = 10;

        let output = vec![0u8; 1024];
        let writer = Cursor::new(output);
        let mut encoder = Encoder::new(writer, width as u32, height as u32);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Rgb);
        let mut png_writer = encoder.write_header()?;

        let correct_image_size = width * height * 3;
        let image = vec![0u8; correct_image_size + 1];
        let result = png_writer.write_image_data(image.as_ref());
        assert!(result.is_err());

        Ok(())
    }

    #[test]
    fn expect_error_on_empty_image() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let encoder = Encoder::new(&mut writer, 0, 0);
        assert!(encoder.write_header().is_err());

        let encoder = Encoder::new(&mut writer, 100, 0);
        assert!(encoder.write_header().is_err());

        let encoder = Encoder::new(&mut writer, 0, 100);
        assert!(encoder.write_header().is_err());

        Ok(())
    }

    #[test]
    fn expect_error_on_invalid_bit_depth_color_type_combination() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_err());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_err());

        Ok(())
    }

    #[test]
    fn can_write_header_with_valid_bit_depth_color_type_combination() -> Result<()> {
        use std::io::Cursor;

        let output = vec![0u8; 1024];
        let mut writer = Cursor::new(output);

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::One);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Two);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Four);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Indexed);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Eight);
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Grayscale);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Rgb);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::GrayscaleAlpha);
        assert!(encoder.write_header().is_ok());

        let mut encoder = Encoder::new(&mut writer, 1, 1);
        encoder.set_depth(BitDepth::Sixteen);
        encoder.set_color(ColorType::Rgba);
        assert!(encoder.write_header().is_ok());

        Ok(())
    }

    #[test]
    fn all_filters_roundtrip() -> io::Result<()> {
        let pixel: Vec<_> = (0..48).collect();

        let roundtrip = |filter: FilterType| -> io::Result<()> {
            let mut buffer = vec![];
            let mut encoder = Encoder::new(&mut buffer, 4, 4);
            encoder.set_depth(BitDepth::Eight);
            encoder.set_color(ColorType::Rgb);
            encoder.set_filter(filter);
            encoder.write_header()?.write_image_data(&pixel)?;

            let decoder = crate::Decoder::new(io::Cursor::new(buffer));
            let (info, mut reader) = decoder.read_info()?;
            assert_eq!(info.width, 4);
            assert_eq!(info.height, 4);
            let mut dest = vec![0; pixel.len()];
            reader.next_frame(&mut dest)?;
            assert_eq!(dest, pixel, "Deviation with filter type {:?}", filter);

            Ok(())
        };

        roundtrip(FilterType::NoFilter)?;
        roundtrip(FilterType::Sub)?;
        roundtrip(FilterType::Up)?;
        roundtrip(FilterType::Avg)?;
        roundtrip(FilterType::Paeth)?;

        Ok(())
    }

    #[test]
    fn some_gamma_roundtrip() -> io::Result<()> {
        let pixel: Vec<_> = (0..48).collect();

        let roundtrip = |gamma: Option<ScaledFloat>| -> io::Result<()> {
            let mut buffer = vec![];
            let mut encoder = Encoder::new(&mut buffer, 4, 4);
            encoder.set_depth(BitDepth::Eight);
            encoder.set_color(ColorType::Rgb);
            encoder.set_filter(FilterType::Avg);
            if let Some(gamma) = gamma {
                encoder.set_source_gamma(gamma);
            }
            encoder.write_header()?.write_image_data(&pixel)?;

            let decoder = crate::Decoder::new(io::Cursor::new(buffer));
            let (info, mut reader) = decoder.read_info()?;
            assert_eq!(
                reader.info().source_gamma,
                gamma,
                "Deviation with gamma {:?}",
                gamma
            );
            assert_eq!(info.width, 4);
            assert_eq!(info.height, 4);
            let mut dest = vec![0; pixel.len()];
            reader.next_frame(&mut dest)?;

            Ok(())
        };

        roundtrip(None)?;
        roundtrip(Some(ScaledFloat::new(0.35)))?;
        roundtrip(Some(ScaledFloat::new(0.45)))?;
        roundtrip(Some(ScaledFloat::new(0.55)))?;
        roundtrip(Some(ScaledFloat::new(0.7)))?;
        roundtrip(Some(ScaledFloat::new(1.0)))?;
        roundtrip(Some(ScaledFloat::new(2.5)))?;

        Ok(())
    }

    /// A Writer that only writes a few bytes at a time
    struct RandomChunkWriter<R: Rng, W: Write> {
        rng: R,
        w: W,
    }

    impl<R: Rng, W: Write> Write for RandomChunkWriter<R, W> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            // choose a random length to write
            let len = cmp::min(self.rng.gen_range(1, 50), buf.len());

            self.w.write(&buf[0..len])
        }

        fn flush(&mut self) -> io::Result<()> {
            self.w.flush()
        }
    }
}

/// Mod to encapsulate the converters depending on the `deflate` crate.
///
/// Since this only contains trait impls, there is no need to make this public, they are simply
/// available when the mod is compiled as well.
impl crate::common::Compression {
    fn to_options(self) -> deflate::CompressionOptions {
        match self {
            Compression::Default => deflate::CompressionOptions::default(),
            Compression::Fast => deflate::CompressionOptions::fast(),
            Compression::Best => deflate::CompressionOptions::high(),
            Compression::Huffman => deflate::CompressionOptions::huffman_only(),
            Compression::Rle => deflate::CompressionOptions::rle(),
        }
    }
}
