use image::RgbaImage;

fn main() {
    let mut img = RgbaImage::new(1920, 1080);
    let tile = image::open("examples/scaleup/tinycross.png")
        .unwrap()
        .to_rgba8();

    image::imageops::tile(&mut img, &tile);
    img.save("tiled_wallpaper.png").unwrap();
}
