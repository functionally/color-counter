{-|
Module      :  Vision.Image.IO.Capture
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Image capture.
-}


module Vision.Image.IO.Capture (
-- * Input/output
  captureRGB
) where


import Data.List.Split (chunksOf)
import Foreign.Storable (peekElemOff)
import Graphics.V4L2 (Direction(Capture), ImageFormat(..), PixelFormat(PixelRGB24), imagePixelFormat, getFormat, setFormat, withDevice, withFrame)
import Vision.Image (RGB, RGBPixel(..))
import Vision.Image.Type (Manifest(..))
import Vision.Primitive.Shape (ix2)

import qualified Data.Vector.Storable as V (fromList)


-- | Capture an image from a Video for Linux device.
captureRGB :: FilePath -- ^ The device name.
           -> IO RGB   -- ^ An action to capture one image frame.
captureRGB deviceName =
  do
    withDevice deviceName $ \device -> do
      format <- setFormat device Capture . (\format -> format {imagePixelFormat = PixelRGB24}) =<< getFormat device Capture
      let
        w = imageWidth format
        h = imageHeight format
      withFrame device format $ \p n -> do
        raw <- mapM (peekElemOff p) [0..(n - 1)]
        return
          Manifest
            {
              manifestSize   = h `ix2` w
            , manifestVector = V.fromList $ map (\[r, g, b] -> RGBPixel r g b) $ chunksOf 3 raw
            }

