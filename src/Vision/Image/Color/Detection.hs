{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeFamilies     #-}


module Vision.Image.Color.Detection (
  ColorConfiguration(..)
, ColorSpecification(..)
, NormalizedConfiguration
, NormalizedSpecification
, normalizeConfiguration
, normalizeSpecification
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


data ColorConfiguration a =
  ColorConfiguration
  {
    colorSpecifications  :: [ColorSpecification a]
  , svgDefault           :: String
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


data NormalizedConfiguration a =
  NormalizedConfiguration
  {
    normalizedSpecifications :: [NormalizedSpecification a]
  , normalizedDefault        :: (String, RGBPixel)
  }


normalizeConfiguration :: (InnerSpace a, Floating (Scalar a)) => ColorConfiguration a -> NormalizedConfiguration a
normalizeConfiguration ColorConfiguration{..} =
  NormalizedConfiguration
    {
      normalizedSpecifications = map normalizeSpecification colorSpecifications
    , normalizedDefault        = normalizeColor svgDefault
    }


data ColorSpecification a =
  ColorSpecification
  {
    svgColor       :: String
  , labVertex     :: (a, a, a)
  , labDirection  :: (a, a, a)
  , labThreshold  :: a
  , efficiency    :: a
  }
    deriving (Eq, Generic, Ord, Read, Show)

instance (FromJSON a, Generic a) => FromJSON (ColorSpecification a)

instance (ToJSON a, Generic a) => ToJSON (ColorSpecification a) where
  toJSON = genericToJSON defaultOptions


data NormalizedSpecification a =
  NormalizedSpecification
  {
    normalizedColor      :: (String, RGBPixel)
  , normalizedVertex     :: (a, a, a)
  , normalizedDirection  :: (a, a, a)
  , normalizedThreshold  :: a
  }


normalizeSpecification :: (InnerSpace a, Floating (Scalar a)) => ColorSpecification a -> NormalizedSpecification a
normalizeSpecification ColorSpecification{..} =
  NormalizedSpecification
    {
      normalizedColor     = normalizeColor svgColor
    , normalizedVertex    = labVertex
    , normalizedDirection = normalized labDirection
    , normalizedThreshold = labThreshold
    }


normalizeColor :: String -> (String, RGBPixel)
normalizeColor =
  ap (,)
    $ uncurryRGB RGBPixel
    . toSRGB24
    . fromJust
    . (readColourName :: String -> Maybe (Colour Double))


quantize :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a) => ColorConfiguration a -> RGB -> RGB
quantize colorConfiguration =
  I.map
    $ snd
    . snd
    . classify (normalizeConfiguration colorConfiguration)


pixels :: RGB -> [RGBPixel]
pixels = V.toList . manifestVector


analyze :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a) => ColorConfiguration a -> RGB -> [(RGBPixel, (a, a, a), String)]
analyze colorConfiguration image =
  [
    (p, lab, color)
  |
    p <- pixels image
  , let (lab, (color, _)) = classify normalizedConfiguration p
  ]
    where
      normalizedConfiguration = normalizeConfiguration colorConfiguration


tally :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a) => ColorConfiguration a -> RGB -> [(String, Int)]
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


effectiveTally :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a) => ColorConfiguration a -> RGB -> [(String, a)]
effectiveTally colorConfiguration@ColorConfiguration{..} =
  zipWith (\ColorSpecification{..} (color, count) -> (color, fromIntegral count / efficiency)) colorSpecifications
    . tally colorConfiguration
    


classify :: (AffineSpace a, InnerSpace a, RealFloat a, a ~ Scalar (a, a, a), a ~ Diff a) => NormalizedConfiguration a -> RGBPixel -> ((a, a, a), (String, RGBPixel))
classify NormalizedConfiguration{..} RGBPixel{..} =
  let
    lab = cieLABView d65 $ sRGB24 rgbRed rgbGreen rgbBlue
  in
    (lab, )
      $ snd
      $ maximumBy (compare `on` fst)
      $ (++ [(-100000, normalizedDefault)])
      $ mapMaybe (measure lab) normalizedSpecifications


measure :: (AffineSpace a, InnerSpace a, Ord a, a ~ Scalar (a, a, a), a ~ Diff a) => (a, a, a) -> NormalizedSpecification a -> Maybe (a, (String, RGBPixel))
measure lab NormalizedSpecification{..} =
  let
    distance = (lab .-. normalizedVertex) <.> normalizedDirection
  in
    if distance > normalizedThreshold
      then Just (distance, normalizedColor)
      else Nothing


{- ORIGINAL VERSION:
classify :: RGBPixel -> Classification
classify RGBPixel{..} =
  let
    normalize (a, b) =
      (a' / n, b' / n)
        where
          a' = a - a0
          b' = b - b0
          n = sqrt $ a' * a' + b' * b'
    (a0, b0) = (0, 5)
    green0  = normalize (-50,  30)
    yellow0 = normalize (  0,  80)
    blue0   = normalize ( 10, -50)
    red0    = normalize ( 70,  50)
    (l, a, b) = cieLABView d65 $ sRGB24 rgbRed rgbGreen rgbBlue :: (Double, Double, Double)
    a' = a - a0
    b' = b - b0
    test (a'', b'') = a' * a'' + b' * b''
    x@[green, yellow, blue, red] = map test [green0, yellow0, blue0, red0]
    best = maximum x
  in
    if best < 15
      then if l < 25
        then Black
        else Gray
      else if green == best
        then Green
        else if yellow == best
          then Yellow
          else if blue == best
            then Blue
            else if red == best
              then Red
              else Gray
-}
