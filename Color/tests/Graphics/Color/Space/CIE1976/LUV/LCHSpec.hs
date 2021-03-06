{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.CIE1976.LUV.LCHSpec (spec) where

import Data.Coerce ( coerce )
import Graphics.Color.Model ( lxy2lch, lch2lxy )
import Graphics.Color.Space.Common
import Graphics.Color.Space.CIE1976.LUV
import Graphics.Color.Space.CIE1976.LUVSpec ()
import Graphics.Color.Space.CIE1976.LUV.LCH

instance (Elevator e, Random e) => Arbitrary (Color (LCHuv i) e) where
  arbitrary = ColorLCHuv <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "LCHuv" $ do
    colorModelSpec @(LCHuv D65) @Word "LCH"
    colorSpaceSpec @(LCHuv D65) @Double
    prop "luv2lch . lch2luv" $ \(luv :: Color (LUV D65) Double) ->
      luv `epsilonEqColor` lch2luv (luv2lch luv)
    prop "lch2luv . luv2lch" $ \(lch :: Color (LCHuv D65) Double) ->
      lch `epsilonEqColor` luv2lch (lch2luv lch)

luv2lch :: Illuminant i => Color (LUV i) Double -> Color (LCHuv i) Double
luv2lch = coerce . lxy2lch . toComponents

lch2luv :: Illuminant i => Color (LCHuv i) Double -> Color (LUV i) Double
lch2luv = fromComponents . lch2lxy . coerce
