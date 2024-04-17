# DeserialImage
This Python module provides a simple interface to decode the image data of the
`DynamicImage` object from the [`serialimage`](https://crates.io/crates/serialimage)
crate through the `DeserialImage` class.

## Installation
```bash
$ pip install deserialimage@https://github.com/sunipkm/serialimage/tree/master/python
```

## Usage
```python
from deserialimage import DeserialImage

# Create a DeserialImage object
json_str = ... # JSON string from the `serialimage` crate
di = DeserialImage::from_json(json_str) # Create a DeserialImage object from the JSON string
image = di.data # Get the image data
...
```