Count colors in images
======================

This Haskell package contains functions for counting colors in images, either from a file or a camera feed.  The input image must in a standard format like JPEG or PNG.  The analyze function outputs the RGB and CIE-LAB values for each pixel, along with the color detected there.  The tally function outputs a histogram of the colors detected.  The quantize function outputs an image where the pixels have been replaced by the colors detected there.

Please report issues at <<https://bwbush.atlassian.net/projects/HCC/issues/>>.


Calibration
-----------

The calibration parameters are stored in the `ColorConfiguration` data type of the `Vision.Image.Color.Detection` module.  It contains a default color and a list of `ColorSpecificiation` entries, one for each color to be detected.  All colors must be specified by their [SVG names](https://www.w3.org/TR/SVG/types.html#ColorKeywords).

The `ColorSpecification` entries define the color being detected and a half plane in [CIE-LAB color space](https://en.wikipedia.org/wiki/Lab_color_space#CIELAB).  The color is considered "detected" when it lies deeper than the specified threshold into the half plane defined by the specified vertex and direction.  (The threshold parameter is redundant mathematically with the vertex location, but it is convenient to have such a parameter to make it easy to make minor adjustments to calibration.)  When an observation lies in the half planes for several colors, it is assigned to the one in whose half plane it is deepest.  We have found that calibration using this method of CIE-LAB half planes is robust under widely different lighting conditions.

Because the detection efficiency varies somewhat by color, an efficiency parameter is included in `ColorSpecification`.


Skeletal example illustrating the tallying of colors in an image
----------------------------------------------------------------

```haskell
main :: IO ()
main = do
  input <- readRGB "data/sample.jpg"
  print $ tally def input
  let output = quantize def input
  writeRBG True Nothing Nothing "analysis.png" output
```
