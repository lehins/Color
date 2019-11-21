{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.CIE1976.LABSpec (spec) where

import Graphics.ColorSpace.Common
import Graphics.ColorSpace.CIE1931.Illuminant as I2
import Graphics.ColorSpace.CIE1976.LAB


instance (Elevator e, Random e, Illuminant i) => Arbitrary (Pixel (LAB (i :: k)) e) where
  arbitrary = PixelLAB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "LAB" $ do
  colorModelSpec @(LAB 'D65) @Word
  prop "toFromPixelXYZ" $ prop_toFromPixelXYZ @(LAB 'D65) @Double
  prop "toFromColorSpace" $ prop_toFromColorSpace @(LAB 'D65) @Double

