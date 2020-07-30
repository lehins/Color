{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.AdobeRGBSpec (spec, arbitraryElevator) where

import Graphics.Color.Space.Common

instance (Elevator e, Random e) => Arbitrary (Color (AdobeRGB l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "AdobeRGB" $ do
  colorModelSpec @(AdobeRGB 'NonLinear) @Word "AdobeRGB 'NonLinear"
  colorSpaceLenientSpec @(AdobeRGB 'NonLinear) @Float 0.00001
  colorSpaceLenientSpec @(AdobeRGB 'Linear) @Double 0.00001
