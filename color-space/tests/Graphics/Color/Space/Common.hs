{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Color.Space.Common
  ( module Graphics.Color.Space
  , module Graphics.Color.Model.Common
  , prop_toFromColorXYZ
  , prop_toFromLenientColorXYZ
  , prop_toFromColorSpace
  ) where

import Graphics.Color.Space
import Graphics.Color.Model.Common


prop_toFromColorXYZ ::
     forall cs i e. (ColorSpace cs (i :: k) e, RealFloat e)
  => Color cs e
  -> Property
prop_toFromColorXYZ px = px `epsilonEqColor` fromColorXYZ (toColorXYZ px :: Color (XYZ i) Double)


-- For RGB standards, that have matrices rounded to 4 digits after the decimal point
prop_toFromLenientColorXYZ ::
     forall cs i e. (ColorSpace cs i e, RealFloat e)
  => e
  -> Color cs e
  -> Property
prop_toFromLenientColorXYZ epsilon px =
  epsilonEqColorTol epsilon px (fromColorXYZ (toColorXYZ px :: Color (XYZ i) Double))


prop_toFromColorSpace ::
     forall cs i e. (ColorSpace cs i e, RealFloat e)
  => Color cs e
  -> Property
prop_toFromColorSpace px = px `epsilonEqColor` fromBaseSpace (toBaseSpace px)

