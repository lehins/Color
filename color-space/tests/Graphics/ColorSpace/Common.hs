{-# LANGUAGE RankNTypes #-}
module Graphics.ColorSpace.Common
  ( module Graphics.ColorSpace
  , module Graphics.ColorModel.Common
  , prop_toFromPixelXYZ
  , prop_toFromLenientPixelXYZ
  , prop_toFromColorSpace
  ) where

import Graphics.ColorSpace
import Graphics.ColorModel.Common


prop_toFromPixelXYZ ::
     forall cs e. (ColorSpace cs e, RealFloat e)
  => Pixel cs e
  -> Property
prop_toFromPixelXYZ px = px `epsilonEqPixel` fromPixelXYZ (toPixelXYZ px)


-- For RGB standards, that have matrices rounded to 4 digits after the decimal point
prop_toFromLenientPixelXYZ ::
     forall cs e. (ColorSpace cs e, RealFloat e)
  => e
  -> Pixel cs e
  -> Property
prop_toFromLenientPixelXYZ epsilon px = epsilonEqPixelTol epsilon px (fromPixelXYZ (toPixelXYZ px))


prop_toFromColorSpace :: (ColorSpace cs e, RealFloat e) => Pixel cs e -> Property
prop_toFromColorSpace px = px `epsilonEqPixel` fromBaseColorSpace (toBaseColorSpace px)

