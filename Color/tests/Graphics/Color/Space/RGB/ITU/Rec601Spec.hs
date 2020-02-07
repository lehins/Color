{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.RGB.ITU.Rec601Spec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.ITU.Rec601

instance (Elevator e, Random e) => Arbitrary (Color BT601_525 e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

instance (Elevator e, Random e) => Arbitrary (Color BT601_625 e) where
  arbitrary = ColorRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = do
  describe "Rec601 (525)" $ do
    colorModelSpec @BT601_525 @Word "BT601_525"
    colorSpaceSpec @BT601_525 @Float
  describe "Rec601 (625)" $ do
    colorModelSpec @BT601_625 @Word "BT601_625"
    colorSpaceSpec @BT601_625 @Float
