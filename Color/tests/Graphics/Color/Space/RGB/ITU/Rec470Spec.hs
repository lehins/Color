{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.ITU.Rec470Spec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.ITU.Rec470

instance (Elevator e, Random e) => Arbitrary (Color BT470_525 e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

instance (Elevator e, Random e) => Arbitrary (Color BT470_625 e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = do
  describe "Rec470 (525)" $ do
    colorModelSpec @BT470_525 @Word "BT470_525"
    colorSpaceSpec @BT470_525 @_ @Float
  describe "Rec470 (625)" $ do
    colorModelSpec @BT470_625 @Word "BT470_625"
    colorSpaceSpec @BT470_625 @_ @Float
