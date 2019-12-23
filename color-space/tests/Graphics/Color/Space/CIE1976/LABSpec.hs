{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.CIE1976.LABSpec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Illuminant.CIE1931 as I2
import Graphics.Color.Space.CIE1976.LAB

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (LAB (i :: k)) e) where
  arbitrary = ColorLAB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "LAB" $ do
  colorModelSpec @(LAB 'D65) @Word
  prop "toFromColorXYZ" (prop_toFromColorXYZ :: Color (LAB 'D65) Double -> Property)
  prop "toFromColorSpace" (prop_toFromColorSpace :: Color (LAB 'D65) Double -> Property)

