# Rust Image 0.1 Release Notes

Rust image aims to be a pure-Rust implementation of various popular image formats. Accompanying reading/write support, rust image provides basic imaging processing function. See `README.md` for further details.

## Known issues
 - Interlaced (progressive) or animated images are not supported.
 - Images images with *n* bit/channel (*n ≠ 8*) are not well supported.

## Changes

### Version 0.1
- Initial release
- Basic reading support for png, jpeg, gif, ppm and webp.
- Basic writing support for png and jpeg. 
- A collection of basic imaging processing function like `blur` or `invert`