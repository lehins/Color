{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.RGB.Derived.SRGBSpec (spec) where

import Graphics.ColorSpace.Common
import Graphics.ColorSpace.RGB.Derived.SRGB

instance (Elevator e, Random e) => Arbitrary (Pixel (RGB (i :: k)) e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "SRGB" $ do
  colorModelSpec @(RGB 'D65) @Word
  prop "toFromPixelXYZ" $ prop_toFromPixelXYZ @(RGB 'D65) @Double
  prop "toFromColorSpace" $ prop_toFromColorSpace @(RGB 'D65) @Double
