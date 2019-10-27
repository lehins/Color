{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.RGB.ITU.Rec709Spec (spec) where

import Graphics.ColorSpace.Common
import Graphics.ColorSpace.RGB.ITU.Rec709

instance (Elevator e, Random e) => Arbitrary (Pixel (RGB 'D65) e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "Rec709" $ do
  colorModelSpec @(RGB 'D65) @Word
  -- Roundrtrip is not always very accurate, eg: 8.115324539550295e-2 /= 8.140132075907752e-2
  prop "toFromPixelXYZ (lenient)" $ \ (px :: Pixel (RGB 'D65) Double) ->
    epsilonEqPixelTol 5e-4 px (fromPixelXYZ (toPixelXYZ @_ @_ @Double px))
  prop "toFromColorSpace" $ prop_toFromColorSpace @(RGB 'D65) @Double
