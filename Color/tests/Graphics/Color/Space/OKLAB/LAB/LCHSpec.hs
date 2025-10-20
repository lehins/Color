{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.OKLAB.LAB.LCHSpec (spec) where

import Data.Coerce ( coerce )
import Graphics.Color.Model ( lxy2lch, lch2lxy )
import Graphics.Color.Space.Common
import Graphics.Color.Space.OKLAB.LABSpec ()
import Graphics.Color.Space.OKLAB.LCH
import Graphics.Color.Space.OKLAB

instance (Elevator e, Random e) => Arbitrary (Color OKLCH e) where
  arbitrary = ColorOKLCH <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "OKLCH" $ do
    colorModelSpec @OKLCH @Word "LCH-OKLAB"
    colorSpaceLenientSpec @OKLCH @Double 1e-9
    prop "lab2lch . lch2lab" $ \(lab :: Color OKLAB Double) ->
      lab `epsilonEqColor` lch2lab (lab2lch lab)
    prop "lch2lab . lab2lch" $ \(lch :: Color OKLCH Double) ->
      lch `epsilonEqColor` lab2lch (lch2lab lch)

lab2lch :: Color OKLAB Double -> Color OKLCH Double
lab2lch = coerce . lxy2lch . toComponents

lch2lab :: Color OKLCH Double -> Color OKLAB Double
lch2lab = fromComponents . lch2lxy . coerce
