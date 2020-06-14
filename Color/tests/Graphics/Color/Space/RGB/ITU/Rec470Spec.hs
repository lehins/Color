{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.ITU.Rec470Spec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.ITU.Rec470

instance (Elevator e, Random e) => Arbitrary (Color (BT470_525 l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

instance (Elevator e, Random e) => Arbitrary (Color (BT470_625 l) e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = do
  describe "Rec470 (525)" $ do
    colorModelSpec @(BT470_525 'NonLinear) @Word "BT470_525 'NonLinear"
    colorSpaceSpec @(BT470_525 'NonLinear) @Float
    colorModelSpec @(BT470_525 'Linear) @Word8 "BT470_525 'Linear"
    colorSpaceSpec @(BT470_525 'Linear) @Double
  describe "Rec470 (625)" $ do
    colorModelSpec @(BT470_625 'NonLinear) @Word "BT470_625 'NonLinear"
    colorSpaceSpec @(BT470_625 'NonLinear) @Float
    colorModelSpec @(BT470_625 'Linear) @Word16 "BT470_625 'Linear"
    colorSpaceSpec @(BT470_625 'Linear) @Double
