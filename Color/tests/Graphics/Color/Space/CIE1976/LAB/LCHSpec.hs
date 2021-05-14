{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.CIE1976.LAB.LCHSpec (spec) where

import Data.Coerce ( coerce )
import Graphics.Color.Model ( lxy2lch, lch2lxy )
import Graphics.Color.Space.Common
import Graphics.Color.Space.CIE1976.LABSpec ()
import Graphics.Color.Space.CIE1976.LAB.LCH

instance (Elevator e, Random e) => Arbitrary (Color (LCHab i) e) where
  arbitrary = ColorLCHab <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "LCHab" $ do
    colorModelSpec @(LCHab D65) @Word "LCH"
    colorSpaceSpec @(LCHab D65) @Double
    prop "lab2lch . lch2lab" $ \(lab :: Color (LAB D65) Double) ->
      lab `epsilonEqColor` lch2lab (lab2lch lab)
    prop "lch2lab . lab2lch" $ \(lch :: Color (LCHab D65) Double) ->
      lch `epsilonEqColor` lab2lch (lch2lab lch)

lab2lch :: Illuminant i => Color (LAB i) Double -> Color (LCHab i) Double
lab2lch = coerce . lxy2lch . toComponents

lch2lab :: Illuminant i => Color (LCHab i) Double -> Color (LAB i) Double
lch2lab = fromComponents . lch2lxy . coerce
