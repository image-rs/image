Copyrights:

* `Lohengrin_-_Illustrated_Sporting_and_Dramatic_News.png`: Public Domain, according to Wikimedia
* `kodim*.png`: Eastman Kodak Company, released for unrestricted use
* `Transparency.png`: Public Domain, according to Wikimedia
* `zune-paletted.png`:
  [Creative Commons Attribution-Share Alike 3.0 Unported](https://creativecommons.org/licenses/by-sa/3.0/deed.en).  This is based on
  https://commons.wikimedia.org/wiki/File:Stadt_Onex_2021.png - it has been
  downloaded at 2048px resolution and modified to use `ColorType::Indexed` with
  `convert -type palette -colors 256 2048px-Stadt_Onex_2021.png
  tests/benches/paletted-zune.png`.

The images use different filtering:

* Lohengrin: no filtering
* kodim02: mixed
* kodim07: mainly paeth
* kodim17: mainly sub
* kodim23: mixed
