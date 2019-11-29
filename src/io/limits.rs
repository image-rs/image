/// Limits on how much memory that can be used in the decoding process. These limits are "best
/// effort" limits and there are no strict guarantees that they will be followed at all times.
#[derive(Debug, Copy, Clone)]
pub struct DecodingLimits {
    /// A limit on how large buffers a decoder is allowed to allocate. The default is 512MiB.
    pub buffer_limit: usize,
}

impl Default for DecodingLimits {
    fn default() -> Self {
        DecodingLimits {
            buffer_limit: 512*1024*1024,
        }
    }
}
