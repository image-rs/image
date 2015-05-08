extern crate cocoa;
#[macro_use] extern crate objc;

extern crate png;

use cocoa::base::*;
use cocoa::foundation::*;
use cocoa::appkit::*;

unsafe fn open_window(width: f64, height: f64, data: &[u8]) {
    let _pool = NSAutoreleasePool::new(nil);

    let app = NSApp();
    app.setActivationPolicy_(NSApplicationActivationPolicyRegular);

    // create Menu Bar
    let menubar = NSMenu::new(nil).autorelease();
    let app_menu_item = NSMenuItem::new(nil).autorelease();
    menubar.addItem_(app_menu_item);
    app.setMainMenu_(menubar);

    // create Application menu
    let app_menu = NSMenu::new(nil).autorelease();
    let quit_prefix = NSString::alloc(nil).init_str("Quit ");
    let quit_title = quit_prefix.stringByAppendingString_(
        NSProcessInfo::processInfo(nil).processName()
    );
    let quit_action = selector("terminate:");
    let quit_key = NSString::alloc(nil).init_str("q");
    let quit_item = NSMenuItem::alloc(nil).initWithTitle_action_keyEquivalent_(
        quit_title,
        quit_action,
        quit_key
    ).autorelease();
    app_menu.addItem_(quit_item);
    app_menu_item.setSubmenu_(app_menu);

    // create Window
    let window = NSWindow::alloc(nil).initWithContentRect_styleMask_backing_defer_(
        NSRect::new(NSPoint::new(0., 0.), NSSize::new(width, height)),
        NSTitledWindowMask as NSUInteger,
        NSBackingStoreBuffered,
        NO
    ).autorelease();
    window.cascadeTopLeftFromPoint_(NSPoint::new(20., 20.));
    window.center();
    
    let image_view: id = msg_send![class("NSImageView"), new];
    let _:() = msg_send![window, setContentView: image_view];
    let bounds: NSRect = msg_send![image_view, bounds];
    let bitmap: id = msg_send![image_view, bitmapImageRepForCachingDisplayInRect: bounds];
    let image: id = msg_send![class("NSImage"), new];
    let _:() = msg_send![image, addRepresentation: bitmap];
    let _:() = msg_send![image_view, setImage: image];
    let _:() = msg_send![image, lockFocus];
    for y in 0..(height as i32) {
        for x in 0..(width as i32) {
            let pixel = &data[4*(y*width as i32 + x) as usize..];
            let color: id = msg_send![
                class("NSColor"),
                colorWithCalibratedRed: (pixel[0] as f64/255.) 
                green: (pixel[1] as f64/255.) 
                blue: (pixel[2] as f64/255.) 
                alpha: (pixel[3] as f64/255.)
            ];
            let _:() =  msg_send![color, set];
            let rect = NSRect::new(NSPoint::new(x as f64, y as f64), NSSize::new(1., 1.));
            let _:() = msg_send![class("NSBezierPath"), fillRect: rect];
        }
    }
    let _:() = msg_send![image, unlockFocus];
    
    let title = NSString::alloc(nil).init_str("Display PNG");
    window.setTitle_(title);
    window.makeKeyAndOrderFront_(nil);

    //app.activateIgnoringOtherApps_(YES);
    app.run();
}

fn main() {
    use std::fs::File;
    let mut reader = png::Reader::new(File::open("/Users/nwinter/Desktop/green.png").unwrap());
    let (width, height) = reader.read_info().unwrap().size();
    let mut data = Vec::with_capacity((width*height) as usize);
    while let Some(row) = reader.next_row().unwrap() {
        data.extend(row.iter().map(|&v| v))
    }
    unsafe { open_window(width as f64, height as f64, &data) }
}