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
import System.Directory (doesFileExist, removeFile)
import Vision.Image (RGB)
import Vision.Image.Storage.DevIL (Autodetect(..), StorageError, load, save)


-- | Read an image from a file.
readRGB :: FilePath -- ^ The path to the file.
        -> IO RGB   -- ^ An action for reading the image as RGB.
readRGB path =
  do
    image <- load Autodetect path :: IO (Either StorageError RGB)
    either (error . show) return image


-- | Write an image to a file.
writeRGB :: Bool     -- ^ Whether to overwrite the file if it already exists.
         -> FilePath -- ^ The path to the file.
         -> RGB      -- ^ The image.
         -> IO ()    -- ^ An action to write the image.
writeRGB overwrite path image =
  do
    exists <- doesFileExist path
    when (overwrite && exists)
      $ removeFile path
    status <- save Autodetect path image
    maybe (return ()) (error . show) status
