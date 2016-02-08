{-|
Module      :  Vision.Image.Color.Detection
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Color detection.
-}


{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}


module Vision.Image.Color.Detection (
-- * Configuration
  ColorConfiguration(..)
, ColorSpecification(..)
, OptimizedConfiguration
, OptimizedSpecification
, optimizeConfiguration
, optimizeSpecification
-- * Classification
, classify
, measure
, quantize
, analyze
, tally
, effectiveTally
) where


import Control.Monad (ap)
import Data.Aeson (FromJSON, ToJSON(toJSON), defaultOptions, genericToJSON)
import Data.AffineSpace (AffineSpace, Diff, (.-.))
import Data.Colour.CIE (Colour, cieLABView)
import Data.Colour.CIE.Illuminant (d65)
import Data.Colour.Names (readColourName)
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.SRGB (sRGB24, toSRGB24)
import Data.Default (Default(..))
import Data.Function (on)
import Data.List (maximumBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.VectorSpace (InnerSpace, Scalar, (<.>), normalized)
import GHC.Generics (Generic)
import Vision.Image (RGB, RGBPixel(..), manifestVector)

import qualified Data.Map.Strict as M ((!), fromList, insert, toList)
import qualified Data.Vector.Storable as V (toList)
import qualified Vision.Image.Class as I (map)


-- | Configuration for color detection.
data ColorConfiguration a =
  ColorConfiguration
  {
    colorSpecifications  :: [ColorSpecification a] -- ^ Specification of colors.
  , svgDefault           :: String                 -- ^ Default color.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance (FromJSON a, Generic a) => FromJSON (ColorConfiguration a)

instance (ToJSON a, Generic a) => ToJSON (ColorConfiguration a) where
  toJSON = genericToJSON defaultOptions

instance Num a => Default (ColorConfiguration a) where
  def =
    ColorConfiguration
      [
        ColorSpecification "green"  ( 0, 0, 5) ( 0, -50,  25) 15 1
      , ColorSpecification "yellow" ( 0, 0, 5) ( 0,   0,  75) 15 1
      , ColorSpecification "blue"   ( 0, 0, 5) ( 0,  10, -55) 15 1
      , ColorSpecification "red"    ( 0, 0, 5) ( 0,  70,  45) 15 1
      , ColorSpecification "black"  (25, 0, 0) (-1,   0,   0)  0 1
      ]
      "gray"


-- | An optimized version of a configuration for color detection.
data OptimizedConfiguration a =
  OptimizedConfiguration
  {
    optimizedSpecifications :: [OptimizedSpecification a] -- ^ Optimized versions of the specifications.
  , optimizedDefault        :: (String, RGBPixel)          -- ^ Optimized version of the default color.
  }


-- | Optimize a configuration fo color detection.
optimizeConfiguration :: (InnerSpace a, Floating (Scalar a))
                       => ColorConfiguration a      -- ^ The configuration.
                       -> OptimizedConfiguration a  -- ^ An optimized version of the configuration.
optimizeConfiguration ColorConfiguration{..} =
  OptimizedConfiguration
    {
      optimizedSpecifications = map optimizeSpecification colorSpecifications
    , optimizedDefault        = optimizeColor svgDefault
    }


-- | Specification for detecting a color.
data ColorSpecification a =
  ColorSpecification
  {
    svgColor       :: String   -- ^ The color.
  , labVertex     :: (a, a, a) -- ^ A CIE-LAB point on the separating plane.
  , labDirection  :: (a, a, a) -- ^ A CIE-LAB direction pointing into the half plane.
  , labThreshold  :: a         -- ^ The distance beyond which the color is detected in the half plane.
  , efficiency    :: a         -- ^ The detection efficiency.
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance (FromJSON a, Generic a) => FromJSON (ColorSpecification a)

instance (ToJSON a, Generic a) => ToJSON (ColorSpecification a) where
  toJSON = genericToJSON defaultOptions


-- An optimized version of a specification for detecting a color.
data OptimizedSpecification a =
  OptimizedSpecification
  {
    optimizedColor      :: (String, RGBPixel) -- ^ The color.
  , optimizedVertex     :: (a, a, a)          -- ^ A CIE-LAB point on the separating plane.
  , optimizedDirection  :: (a, a, a)          -- ^ A CIE-LAB direction pointing into the half plane.
  , optimizedThreshold  :: a                  -- ^ The distance beyond which the color is detected in the half plane.
  }


-- | Optimize a specification for detecting a color.
optimizeSpecification :: (InnerSpace a, Floating (Scalar a))
                      => ColorSpecification a     -- ^ The specification.
                      -> OptimizedSpecification a -- ^ An optimized version of the specification.
optimizeSpecification ColorSpecification{..} =
  OptimizedSpecification
    {
      optimizedColor     = optimizeColor svgColor
    , optimizedVertex    = labVertex
    , optimizedDirection = normalized labDirection
    , optimizedThreshold = labThreshold
    }


-- | Optimize the representation of a color.
optimizeColor :: String             -- ^ The name of the color.
              -> (String, RGBPixel) -- ^ The name of the color and its RGB representation.
optimizeColor =
  ap (,)
    $ uncurryRGB RGBPixel
    . toSRGB24
    . fromJust
    . (readColourName :: String -> Maybe (Colour Double))


-- | Replace the colors in an image with the colors detected there.
quantize :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a)
         => ColorConfiguration a -- ^ The configuration for detecting the colors.
         -> RGB                  -- ^ The original image.
         -> RGB                  -- ^ The recolored image.
quantize colorConfiguration =
  I.map
    $ snd
    . snd
    . classify (optimizeConfiguration colorConfiguration)


-- | Retrieve the pixels in an image.
pixels :: RGB        -- ^ The image.
       -> [RGBPixel] -- ^ The pixels.
pixels = V.toList . manifestVector


-- | List the colors and their detection in an image.
analyze :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a)
        => ColorConfiguration a            -- ^ The configuration for detecting the colors.
        -> RGB                             -- ^ The image.
        -> [(RGBPixel, (a, a, a), String)] -- ^ The RGB value, CIE-LAB value, and detected color for the pixels in the image.
analyze colorConfiguration image =
  [
    (p, lab, color)
  |
    p <- pixels image
  , let (lab, (color, _)) = classify optimizedConfiguration p
  ]
    where
      optimizedConfiguration = optimizeConfiguration colorConfiguration


-- | Tally the colors detected in an image.
tally :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a)
      => ColorConfiguration a -- ^ The configuration for detecting the colors.
      -> RGB                  -- ^ The image.
      -> [(String, Int)]      -- ^ The detected colors and the number of times they occur in the image.
tally colorConfiguration@ColorConfiguration{..} =
  let
    zero = M.fromList $ map (, 0) $ (svgDefault :) $ map svgColor colorSpecifications
    visit counts (_, _, color) =
      let
        count = counts M.! color
      in
        M.insert color (count + 1) counts
  in
    M.toList
      . foldl visit zero
      . analyze colorConfiguration


-- | Tally the colors detected in an image, including correction for detection efficiency.
effectiveTally :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a)
               => ColorConfiguration a -- ^ The configuration for detecting the colors.
               -> RGB                  -- ^ The image.
               -> [(String, a)]        -- ^ The detected colors and the number of times they occur in the image.
effectiveTally colorConfiguration@ColorConfiguration{..} =
  zipWith (\ColorSpecification{..} (color, count) -> (color, fromIntegral count / efficiency)) colorSpecifications
    . tally colorConfiguration
    

-- | Detect the color of a pixel.
classify :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a)
         => OptimizedConfiguration a        -- ^ The configuration for detecting the colors.
         -> RGBPixel                        -- ^ The pixel.
         -> ((a, a, a), (String, RGBPixel)) -- ^ The CIE-LAB value, the detected color, and the pixel.
classify OptimizedConfiguration{..} RGBPixel{..} =
  let
    lab = cieLABView d65 $ sRGB24 rgbRed rgbGreen rgbBlue
  in
    (lab, )
      $ snd
      $ maximumBy (compare `on` fst)
      $ (++ [(-100000, optimizedDefault)])
      $ mapMaybe (measure lab) optimizedSpecifications


-- | Measure the distance of a pixel into a half plan for a color.
measure :: (AffineSpace a, InnerSpace a, Ord a, a ~ Scalar (a, a, a), a ~ Diff a)
        => (a, a, a)                     -- ^ The pixel's CIE-LAB value.
        -> OptimizedSpecification a      -- ^ The specification for the color.
        -> Maybe (a, (String, RGBPixel)) -- ^ The distance into the half plane, the color, and its pixel representation.
measure lab OptimizedSpecification{..} =
  let
    distance = (lab .-. optimizedVertex) <.> optimizedDirection
  in
    if distance > optimizedThreshold
      then Just (distance, optimizedColor)
      else Nothing
