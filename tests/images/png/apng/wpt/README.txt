APNG dispose/blend reference tests copied or derived from Web Platform Tests.

WPT source: https://github.com/web-platform-tests/wpt/tree/b63305b743ed9ce2725d4ae09c5c8f0c40d8e6e1/png/apng

Credits:
- Chris Lilley, main author of the WPT PNG/APNG tests.
- Philip Taylor, author of the earlier APNG testsuite that many WPT APNG tests
  were ported from.

These files cover all APNG dispose/blend operation combinations:

WPT files copied as-is:
- support/013.png -> fcTL-dispose-in-region-none.png
- support/015.png -> fcTL-dispose-in-region-background.png
- support/016.png -> fcTL-dispose-in-region-previous.png
- support/018.png -> fcTL-blend-source-transparent.png

The current WPT APNG files cover none+over, none+source, background+over, and
previous+over. The background+source and previous+source combinations do not
exist in WPT, so these two files are derived:

- dispose-background-blend-source.png
- dispose-previous-blend-source.png

The derived files are minimal 3x1 APNGs. Each has an opaque red base frame, a
half-transparent target frame using the dispose/blend operation named by the
file, and a marker frame that makes the target frame's disposal result visible.

The reference frames in tests/reference/png/apng/wpt/ were generated with APNG
Disassembler 2.9 (apngdis), independently of this crate's PNG decoder.
