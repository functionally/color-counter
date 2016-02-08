{-|
Module      :  Main
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Main entry.
-}


{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE RecordWildCards     #-}


module Main (
-- * Main entry.
  main
) where


import Control.Monad (when)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Data.Yaml (decodeFile, encodeFile)
import Paths_color_counter (version)
import System.Console.CmdArgs ((&=), argPos, auto, cmdArgs, def, details, help, modes, name, opt, program, summary, typ, typFile)
import Vision.Image (RGBPixel(..))
import Vision.Image.IO (readRGB, writeRGB)
import Vision.Image.IO.Capture (captureRGB)

import qualified Data.Default as D (def)
import qualified Vision.Image.Color.Detection as C (ColorConfiguration, analyze, quantize, effectiveTally)


stringVersion :: String
stringVersion = showVersion version ++ " (2016)"


main :: IO ()
main =
  do
    command <- cmdArgs imager
    dispatch command


imager :: Imager
imager =
  modes
    [
      process
    , defaults
    , capture
    ]
      &= program "color-counter"
      &= summary ("Color Counter, Version " ++ stringVersion)
      &= help "This tool detects and counts colors in an image or in a camera feed."


data Imager =
    Process
    {
      configuration :: FilePath
    , input         :: FilePath
    , device        :: Bool
    , analyze       :: FilePath
    , tally         :: FilePath
    , quantize      :: FilePath
    }
  | Defaults
    {
      configuration :: FilePath
    }
  | Capture
    {
      input         :: FilePath
    , output        :: FilePath
    }
    deriving (Data, Show, Typeable)


process :: Imager
process =
  Process
  {
    configuration  = def
                  &= typFile
                  &= help "YAML or JSON configuration file"
  , input          = def
                  &= opt "/dev/stdin"
                  &= typ "INPUT_IMAGE"
                  &= argPos 0
  , device         = def
                  &= typ "BOOLEAN"
                  &= help "the input is a Video for Linux device instead of a file"
  , analyze        = def
                  &= opt "/dev/stdout"
                  &= typFile
                  &= help "tab-separated-value file for pixel-by-pixel analysis"
  , tally          = def
                  &= opt "/dev/stdout"
                  &= typFile
                  &= help "tab-separated-value file for counts of pixels, by color"
  , quantize       = def
                  &= typFile
                  &= help "image file with quantized colors"
  }
    &= name "process"
    &= help "Process an image."
    &= details ["The input image must in a standard format like JPEG or PNG.  The analyze flag outputs the RGB and CIE-LAB values for each pixel, along with the color detected there.  The tally flag outputs a histogram of the colors detected.  The quantize flag outputs an image where the pixels have been replaced by the colors detected there.  The file extension must be that of a common format like PNG."]
    &= auto


defaults :: Imager
defaults =
  Defaults
  {
    configuration  = def
                  &= opt "/dev/stdout"
                  &= typFile
                  &= help "YAML or JSON configuration file"
  }
    &= name "defaults"
    &= help "Write a default configuration file."
    &= details ["The default configuration file is written in YAML format.  SVG names are used to specify colors.  The 'svgDefault' field specifies the color to be used when no color is detected.  Each of the entries in 'colorSpecifications' specifies how to detect the color given by 'svgColor'.  The 'labVertex' and 'labDirection' fields specify a half plane in CIE LAB color space.  The given color is considered to be detected with probability 'efficiency' if its distance into the half plane is at least 'labThreshold'."]


capture :: Imager
capture =
  Capture
  {
    input          = def
                  &= typ "DEVICE"
                  &= argPos 0
  , output         = def
                  &= typ "OUTPUT_IMAGE"
                  &= argPos 1
  }
    &= name "capture"
    &= help "Capture an image from a device."
    &= details ["The input must be a Video for Linux devices such as /dev/video0.  The file extension for the output must be that of a common format like PNG."]


dispatch :: Imager -> IO ()

dispatch Process{..} =
  do
    Just configuration' <- if null configuration then return (Just D.def) else decodeFile configuration :: IO (Maybe (C.ColorConfiguration Double))
    image <-
      if device
        then captureRGB input
        else readRGB input
    when (not $ null analyze)
      $ writeFile analyze
      $ unlines
      $ (("Red" +++ "Green" +++ "Blue" +++ "L" +++ "A" +++ "B" +++ "Color") :)
      $ map (\(RGBPixel{..}, (l, a, b), color) -> show rgbRed +++ show rgbGreen +++ show rgbBlue +++ show l +++ show a +++ show b +++ color)
      $ C.analyze configuration' image
    when (not $ null tally)
      $ writeFile tally
      $ unlines
      $ (("Color" +++ "Pixels") :)
      $ map (\(color, count) -> color +++ show count)
      $ C.effectiveTally configuration' image
    when (not $ null quantize)
      $ writeRGB True quantize
      $ C.quantize configuration' image

dispatch Defaults{..} =
  do
    encodeFile configuration $ (D.def :: C.ColorConfiguration Double)

dispatch Capture{..} =
  do
    image <- captureRGB input
    writeRGB True output image


infixr 5 +++
(+++) :: String -> String -> String
x +++ y = x ++ "\t" ++ y
