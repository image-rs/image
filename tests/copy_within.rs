use image::{math::Rect, GenericImage, GrayImage, Luma};

fn check_copy_within_bounds(image: &mut impl GenericImage<Pixel = Luma<u8>>) {
    let original: Vec<_> = image.pixels().collect();
    let pixel = Rect::from_xy_ranges(0..1, 0..1);

    for offset in [4, 5, u32::MAX] {
        assert!(!image.copy_within(Rect { x: offset, ..pixel }, 0, 0));
        assert!(!image.copy_within(Rect { y: offset, ..pixel }, 0, 0));
        assert!(!image.copy_within(pixel, offset, 0));
        assert!(!image.copy_within(pixel, 0, offset));
    }

    for (width, height) in [(u32::MAX, 1), (1, u32::MAX)] {
        let source = Rect {
            x: 1,
            y: 1,
            width,
            height,
        };
        assert!(!image.copy_within(source, 0, 0));
        assert!(!image.copy_within(
            Rect {
                x: 0,
                y: 0,
                ..source
            },
            1,
            1
        ));
    }

    for (width, height) in [(0, 0), (0, 2), (2, 0)] {
        let source = Rect {
            x: 0,
            y: 0,
            width,
            height,
        };
        assert!(image.copy_within(source, 0, 0));
        assert!(image.copy_within(source, 4 - width, 4 - height));
        assert!(image.copy_within(
            Rect {
                x: 4 - width,
                y: 4 - height,
                ..source
            },
            0,
            0,
        ));
        assert!(!image.copy_within(Rect { x: 5, ..source }, 0, 0));
        assert!(!image.copy_within(Rect { y: 5, ..source }, 0, 0));
        assert!(!image.copy_within(source, 5, 0));
        assert!(!image.copy_within(source, 0, 5));
    }

    assert_eq!(image.pixels().collect::<Vec<_>>(), original);
}

#[test]
fn buffer_copy_within_bounds() {
    let mut image = GrayImage::from_fn(4, 4, |x, y| Luma([(x + y * 4) as u8]));
    check_copy_within_bounds(&mut image);
}

#[test]
fn generic_copy_within_bounds() {
    let mut image = GrayImage::from_fn(6, 6, |x, y| Luma([(x + y * 6) as u8]));
    let original = image.clone();
    check_copy_within_bounds(&mut *image.sub_image(Rect::from_xy_ranges(1..5, 1..5)));
    assert_eq!(image, original);
}

fn check_copy_within_empty_image(image: &mut impl GenericImage) {
    let source = Rect {
        x: 0,
        y: 0,
        width: image.width(),
        height: image.height(),
    };
    assert!(image.copy_within(source, 0, 0));
    assert!(!image.copy_within(Rect { x: 1, ..source }, 0, 0));
    assert!(!image.copy_within(Rect { y: 1, ..source }, 0, 0));
    assert!(!image.copy_within(source, 1, 0));
    assert!(!image.copy_within(source, 0, 1));
}

#[test]
fn buffer_copy_within_empty_images() {
    for (width, height) in [(0, 0), (0, 4), (4, 0), (0, u32::MAX), (u32::MAX, 0)] {
        let mut image = GrayImage::new(width, height);
        check_copy_within_empty_image(&mut image);
    }
}

#[test]
fn generic_copy_within_empty_images() {
    for (width, height) in [(0, 0), (0, 4), (4, 0), (0, u32::MAX), (u32::MAX, 0)] {
        let mut image = GrayImage::new(width, height);
        check_copy_within_empty_image(&mut *image.sub_image(Rect {
            x: 0,
            y: 0,
            width,
            height,
        }));
    }
}
