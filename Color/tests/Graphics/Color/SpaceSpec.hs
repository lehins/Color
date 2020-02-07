{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.SpaceSpec (spec) where

import Graphics.Color.Illuminant.CIE1931
import Graphics.Color.Space.Common


instance (Elevator e, Random e) => Arbitrary (Color (CIExyY i) e) where
  arbitrary = ColorCIExy <$> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = do
  describe "XYZ" $ do
    colorModelSpec @(XYZ 'E) @Word "XYZ"
    colorSpaceSpec @(XYZ 'E) @Double
  -- describe "Alpha XYZ" $ do
  --   colorModelSpec @(Alpha (XYZ 'FL1)) @Word "XYZ"
  --   colorSpaceSpec @(Alpha (XYZ 'FL1)) @Double
  describe "CIExyY" $ do
    colorModelSpec @(CIExyY 'D50) @Word "CIExyY"
    colorSpaceSpec @(CIExyY 'D50) @Double
