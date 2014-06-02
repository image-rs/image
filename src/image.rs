use colortype::ColorType;

#[deriving(Show, PartialEq, Eq)]
pub enum ImageError {
	FormatError,
	DimensionError,
	UnsupportedError,
	UnsupportedColor,
	NotEnoughData,
	IoError
}

pub trait ImageDecoder {
        //Return a tuple containing the width and height of the image
        fn dimensions(&mut self) -> ImageResult<(u32, u32)>;

        //Return the color type of the image e.g RGB(8) (8bit RGB)
        fn colortype(&mut self) -> ImageResult<ColorType>;

        //Returns the length in bytes of one decoded row of the image
        fn row_len(&mut self) -> ImageResult<uint>;

        //Read one row from the image into buf
        //Returns the row index
        fn read_scanline(&mut self, buf: &mut [u8]) -> ImageResult<u32>;

        //Decode the entire image and return it as a Vector
        fn read_image(&mut self) -> ImageResult<Vec<u8>>;
}

pub type ImageResult<T> = Result<T, ImageError>;