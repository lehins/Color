{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.RGB.ITU.Rec709Spec (spec) where

import Graphics.ColorSpace.Common
import Graphics.ColorSpace.RGB.ITU.Rec709

instance (Elevator e, Random e) => Arbitrary (Pixel (RGB 'D65) e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "Rec709" $ do
  colorModelSpec @(RGB 'D65) @Word
  prop "toFromPixelXYZ" $ prop_toFromPixelXYZ @(RGB 'D65) @Double
  prop "toFromColorSpace" $ prop_toFromColorSpace @(RGB 'D65) @Double
