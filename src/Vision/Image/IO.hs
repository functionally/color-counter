module Vision.Image.IO (
  readRGB
, writeRGB
) where


import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import Vision.Image (RGB)
import Vision.Image.Storage.DevIL (Autodetect(..), StorageError, load, save)


readRGB :: FilePath -> IO RGB
readRGB path =
  do
    image <- load Autodetect path :: IO (Either StorageError RGB)
    either (error . show) return image


writeRGB :: Bool -> FilePath -> RGB -> IO ()
writeRGB overwrite path image =
  do
    exists <- doesFileExist path
    when (overwrite && exists)
      $ removeFile path
    status <- save Autodetect path image
    maybe (return ()) (error . show) status
