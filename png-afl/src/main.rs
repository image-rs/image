#![feature(test)]
#![feature(plugin)]
#![plugin(afl_coverage_plugin)]

extern crate afl_coverage;
extern crate test;
extern crate png;

use png::HasParameters;
use std::io::{self, Read};

fn main() {
    let mut input = Vec::new();
    io::stdin().read_to_end(&mut input).unwrap();
    let mut decoder = png::Decoder::new(&*input);
    /*let file = ::std::fs::File::open(
        "fuzzer_out/crashes/id:000002,sig:04,src:000005,op:flip1,pos:43"
    ).unwrap();
    let mut decoder = png::Decoder::new(file);*/
    match (|| -> Result<(), png::DecodingError> {
        let (info, mut reader) = try!(decoder.read_info());
        println!("width = {}, height = {}", info.width, info.height);
        if info.buffer_size() > 50_000_000 {
            return Ok(())
        }
        let mut img_data = vec![0; info.buffer_size()];
        let frame = try!(reader.next_frame(&mut img_data));
        println!("frame 1: {:?}", frame);
        Ok(())
    })() {
        Ok(_) => (),
        Err(err) => println!("{:?}", err)
    }
}