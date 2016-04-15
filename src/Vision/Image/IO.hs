{-|
Module      :  Vision.Image.IO
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Image input/output.
-}


module Vision.Image.IO (
-- * Input/output
  readRGB
, writeRGB
) where


import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, removeFile)
import Vision.Image (RGB)
import Vision.Image.Type (manifestSize)
import Vision.Image.Storage.DevIL (Autodetect(..), StorageError, load, save)
import Vision.Image.Transform (InterpolMethod(TruncateInteger), resize)
import Vision.Primitive.Shape -- (Z, (:.), ix2)


-- | Read an image from a file.
readRGB :: FilePath  -- ^ The path to the file.
        -> IO RGB    -- ^ An action for reading the image as RGB.
readRGB path =
  do
    image <- load Autodetect path :: IO (Either StorageError RGB)
    either (error . show) return image


-- | Write an image to a file.
writeRGB :: Bool      -- ^ Whether to overwrite the file if it already exists.
         -> Maybe Int -- ^ The width for the output image.
         -> Maybe Int -- ^ The height for the output image.
         -> FilePath  -- ^ The path to the file.
         -> RGB       -- ^ The image.
         -> IO ()     -- ^ An action to write the image.
writeRGB overwrite width height path image =
  do
    exists <- doesFileExist path
    let
      size' = manifestSize image
      Z :. height' :. width' = size'
      size = fromMaybe height' height `ix2` fromMaybe width' width
      image' = if size == size' then image else resize TruncateInteger size image
    when (overwrite && exists)
      $ removeFile path
    status <- save Autodetect path image'
    maybe (return ()) (error . show) status
