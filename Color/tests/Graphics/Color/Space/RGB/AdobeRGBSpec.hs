{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.AdobeRGBSpec (spec, arbitraryElevator) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.AdobeRGB

instance (Elevator e, Random e) => Arbitrary (Color AdobeRGB e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "AdobeRGB" $ do
  colorModelSpec @AdobeRGB @Word "AdobeRGB"
  colorSpaceLenientSpec @AdobeRGB @_ @Float 0.00001
