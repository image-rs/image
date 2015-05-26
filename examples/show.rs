#![feature(collections)]

extern crate tcl;
extern crate rust_tcl_sys;
extern crate libc;
extern crate png;
extern crate glob;

use std::ptr;
use std::mem;
use std::env;
use std::io;
use std::path;
use std::fs::File;
use std::ffi::CString;

use tcl::Object;

use rust_tcl_sys::tcl::{ClientData, Tcl_Interp, Tcl_Obj, Tcl_CreateObjCommand};
use rust_tcl_sys::shims::{TCL_OK};
use libc::c_int;

const CREATE_WINDOW: &'static str = r##"
package require Tk

set title "test"
wm protocol . WM_DELETE_WINDOW {
    exit
}
bind . <Escape> {exit}
bind . <Right> {next_image} 

set image [image create photo] 

next_image

wm geometry . [join [list $width "x" $height] ""]
$image put $image_data
 
label .l -image $image
pack .l
raise .
vwait forever
"##;

const LOAD_IMAGE: &'static str = r##"
wm geometry . [join [list $width "x" $height] ""]
wm geometry . "800x600"
wm title . $title 
$image put $image_data
"##;


const BG_COLOR: [u8; 3] = [0xF0, 0xF0, 0xF0];

fn blend(rgba: &[u8], rgb: &[u8]) -> [u8; 3] {
    let alpha = rgba[3] as f32/255.0;
    let mut res = [0; 3];
    for ((&a, &b), c) in rgba[..3].iter().zip(rgb.iter()).zip(res.iter_mut()) {
        *c = (alpha * a as f32 + (1.0 - alpha) * b as f32) as u8
    }
    res
}

fn load_image<'env>(path: &path::PathBuf, env: &'env tcl::TclEnvironment, interp: &mut tcl::Interpreter)
-> Result<((i32, i32), Object<'env>), png::DecodingError> {
    let mut data = Object::new(&env, ());
    
    let decoder = png::Decoder::new(try!(File::open(path)));
    let (info, mut reader) = try!(decoder.read_info());
    let mut img_data = vec![0; info.buffer_size()];
    try!(reader.next_frame(&mut img_data));
    for row in img_data.chunks(info.width as usize * info.color_type.samples()) {
        use png::ColorType::*;
        let mut row_data = Object::new(&env, ());
        match info.color_type {
            RGBA => for rgba in row.chunks(4) {
                let rgb = blend(rgba, &BG_COLOR);
                let str = Object::new(&env, &*format!("#{:02X}{:02X}{:02X}", rgb[0], rgb[1], rgb[2]));
                interp.list_append(&mut row_data, &str);
            },
            RGB => for rgb in row.chunks(3) {
                let str = Object::new(&env, &*format!("#{:02X}{:02X}{:02X}", rgb[0], rgb[1], rgb[2]));
                interp.list_append(&mut row_data, &str);
            },
            Grayscale => for g in row {
                let str = Object::new(&env, &*format!("#{:02X}{:02X}{:02X}", g, g, g));
                interp.list_append(&mut row_data, &str);
            },
            GrayscaleAlpha => for ga in row.chunks(2) {
                let rgb = blend(&[ga[0], ga[0], ga[0], ga[1]], &BG_COLOR);
                let str = Object::new(&env, &*format!("#{:02X}{:02X}{:02X}", rgb[0], rgb[1], rgb[2]));
                interp.list_append(&mut row_data, &str);
            },
            _ => unimplemented!()
        }
        
        interp.list_append(&mut data, &row_data);
    }
    Ok(((info.width as i32, info.height as i32), data))
}

extern "C"
fn next_image(state: ClientData, _: *mut Tcl_Interp, _: c_int, _: *const Tcl_Obj) -> c_int {
    //use tcl::TclResult::TCL_OK;
    //let interp: &mut tcl::Interpreter = unsafe { mem::transmute(interp) };
    let state: &mut State = unsafe { mem::transmute(state) };
    
    let env = state.env;
    let ref mut interp = state.interp;
    
    if state.files.len() < 1 {
        return 0
    }
    
    let tail = state.files.split_off(1);
    let first = mem::replace(&mut state.files, tail);
    
    if let Ok(((width, height), image_data)) = load_image(
        //"tests/samples/PNG_transparency_demonstration_1.png",
        //"tests/samples/lenna_fragment_interlaced.png",
        //"tests/pngsuite/basn2c16.png",
        //"tests/pngsuite/basi3p04.png",
        //"tests/pngsuite/tbbn2c16.png",
        //"tests/pngsuite/tbrn2c08.png",
        //"tests/pngsuite/tbbn3p08.png",
        &first[0],
        &env, interp
    ) {
        interp.set_object_variable(
            &env.new_object("image_data"),
            &image_data,
            tcl::SetVariableScope::Standard,
            tcl::LeaveError::Yes,
            tcl::AppendStyle::Replace
        );
        interp.set_variable("width", width);
        interp.set_variable("height", height);
        interp.set_variable("title", &*first[0].to_string_lossy());
        
        match interp.eval(LOAD_IMAGE, tcl::EvalScope::Local) {
            tcl::TclResult::Ok => TCL_OK,
            _ => {
                -1
            }
        }
    } else {
        -1
    }
}

fn crate_commands(state: &mut State) {
    let name = CString::new("next_image").unwrap();
    unsafe {
        Tcl_CreateObjCommand(
            state.interp.raw(),
            name.as_ptr(),
            mem::transmute(next_image),
            mem::transmute(state),
            ptr::null_mut()
        );
    }
}

struct State<'a, 'env: 'a> {
    env: &'env tcl::TclEnvironment,
    interp: &'a mut tcl::Interpreter<'env>,
    files: Vec<path::PathBuf>
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: show files [...]");
    } else {
        let mut files = vec![];
        for file in args.iter().skip(1) {
            match if file.contains("*") {
                (|| -> io::Result<_> {
                    for entry in try!(glob::glob(&file).map_err(|err| {
                        io::Error::new(io::ErrorKind::Other, err.msg)
                    })) {
                        files.push(try!(entry.map_err(|_| {
                            io::Error::new(io::ErrorKind::Other, "glob error")
                        })))
                    }
                    Ok(())
                })()
            } else {
                files.push(path::PathBuf::from(file));
                Ok(())
            } {
                Ok(_) => (),
                Err(err) => {
                    println!("{}: {}", file, err);
                    break
                }
            }
            
        }
        let env = tcl::init();
        let mut interp = env.interpreter().unwrap();
        let interpp = &mut interp as *mut tcl::Interpreter;
        let mut state = State {
            env: &env,
            interp: &mut interp,
            files: files
        };
        crate_commands(&mut state);
        println!("{:?}", unsafe {&mut *interpp}.eval(CREATE_WINDOW, tcl::EvalScope::Local));
    }
}