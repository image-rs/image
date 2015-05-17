extern crate tcl;
extern crate png;

use std::fs::File;
use tcl::Object;

const CREATE_WINDOW: &'static str = r##"
package require Tk
set title "test"
wm title . $title 
wm geometry . [join [list $width "x" $height] ""]
wm protocol . WM_DELETE_WINDOW {
    exit
}
bind . <Escape> {exit}
 
set image [image create photo] 
$image put $image_data
 
label .l -image $image
pack .l

raise .
vwait forever
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

fn load_image<'env>(path: &str, env: &'env tcl::TclEnvironment, interp: &mut tcl::Interpreter)
-> ((i32, i32), Object<'env>) {
    let mut data = Object::new(&env);
    
    let mut reader = png::Reader::new(File::open(path).unwrap());
    let (width, height) = reader.read_info().unwrap().size();
    let (ct, _) = reader.color_type().unwrap();
    while let Some(row) = reader.next_row().unwrap() {
        use png::ColorType::*;
        let mut row_data = Object::new(&env);
        match ct {
            RGBA => for rgba in row.chunks(4) {
                let rgb = blend(rgba, &BG_COLOR);
                let str = Object::new_string(&env, &format!("#{:02X}{:02X}{:02X}", rgb[0], rgb[1], rgb[2]));
                interp.list_append(&mut row_data, &str);
            },
            RGB => for rgb in row.chunks(3) {
                let str = Object::new_string(&env, &format!("#{:02X}{:02X}{:02X}", rgb[0], rgb[1], rgb[2]));
                interp.list_append(&mut row_data, &str);
            },
            Grayscale => for g in row {
                let str = Object::new_string(&env, &format!("#{:02X}{:02X}{:02X}", g, g, g));
                interp.list_append(&mut row_data, &str);
            },
            GrayscaleAlpha => for ga in row.chunks(2) {
                let rgb = blend(&[ga[0], ga[0], ga[0], ga[1]], &BG_COLOR);
                let str = Object::new_string(&env, &format!("#{:02X}{:02X}{:02X}", rgb[0], rgb[1], rgb[2]));
                interp.list_append(&mut row_data, &str);
            },
            _ => unimplemented!()
        }
        
        interp.list_append(&mut data, &row_data);
    }
    ((width as i32, height as i32), data)
}

fn main() {
    let env = tcl::init();
    let mut interp = env.interpreter().unwrap();
    
    let ((width, height), image_data) = load_image(
        "tests/samples/PNG_transparency_demonstration_1.png", 
        &env, &mut interp
    );
    
    let width = Object::new_integer(&env, width);
    let height = Object::new_integer(&env, height);
    
    interp.set_object_variable(
        &Object::new_string(&env, "image_data"),
        &image_data,
        tcl::SetVariableScope::Standard,
        tcl::LeaveError::Yes,
        tcl::AppendStyle::Replace
    );
    interp.set_object_variable(
        &Object::new_string(&env, "width"),
        &width,
        tcl::SetVariableScope::Standard,
        tcl::LeaveError::Yes,
        tcl::AppendStyle::Replace
    );
    interp.set_object_variable(
        &Object::new_string(&env, "height"),
        &height,
        tcl::SetVariableScope::Standard,
        tcl::LeaveError::Yes,
        tcl::AppendStyle::Replace
    );
    println!("{:?}", interp.eval(CREATE_WINDOW, tcl::EvalScope::Local));
}