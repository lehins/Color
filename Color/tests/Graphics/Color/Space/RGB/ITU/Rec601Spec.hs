{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.ITU.Rec601Spec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.ITU.Rec601

instance (Elevator e, Random e) => Arbitrary (Color (BT601_525 l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

instance (Elevator e, Random e) => Arbitrary (Color (BT601_625 l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = do
  describe "Rec601 (525)" $ do
    colorModelSpec @(BT601_525 'NonLinear) @Word "BT601_525"
    colorSpaceSpec @(BT601_525 'NonLinear) @Float
    colorSpaceSpec @(BT601_525 'Linear) @Double
  describe "Rec601 (625)" $ do
    colorModelSpec @(BT601_625 'NonLinear) @Word "BT601_625"
    colorSpaceSpec @(BT601_625 'NonLinear) @Float
    colorSpaceSpec @(BT601_625 'Linear) @Double
