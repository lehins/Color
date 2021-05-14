{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.CIE1976.LUVSpec (spec) where

import Graphics.Color.Illuminant.CIE1931 as I2
import Graphics.Color.Space.Common
import Graphics.Color.Space.CIE1976.LUV

instance (Elevator e, Random e, Illuminant i) => Arbitrary (Color (LUV (i :: k)) e) where
  arbitrary = ColorLUV <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "LUV" $ do
  colorModelSpec @(LUV 'D65) @Word "LUV"
  colorSpaceLenientSpec @(LUV 'D65) @Double 1e-10
