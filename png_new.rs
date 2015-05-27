struct Decoder {
    /// Previous scanline
    previous: Vec<u8>,
    /// Incomplete line
    current: Vec<u8>,
    current_line_len: usize,
}

impl Decoder {    
    fn update_scanlines(&mut self, mut buf: &[u8], out: &mut [u8]) -> () {
        if self.current.len() > 0 {
            panic!()
        }
        let mut line_len = self.current_line_len;
        let mut previous: &[u8] = &self.previous;
        while buf.len() > line_len {
            let filter = buf[0];
            buf = &buf[1..line_len];
            for pixel in unfilter(filter, previous, buf) {
                
            }
            previous = buf;
        }
        self.current.extend(buf.iter().cloned());
    }
}

struct Pixels {
    i: usize,
    prev: &[u8],
    current: &[u8]
}

fn unfilter<F: Filter>(f: F, prev: &[u8], current: &[u8]) -> Pixels {
    Pixels {
        i: 0,
        prev: prev,
        current: current
    }
}

fn main() {
    println!("hello?");
}