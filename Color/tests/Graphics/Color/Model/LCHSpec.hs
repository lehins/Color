{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.LCHSpec (spec) where

import Graphics.Color.Model
import Graphics.Color.Model.Common

instance (Elevator e, Random e) => Arbitrary (Color LCH e) where
  arbitrary = ColorLCH <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "LCH" $ do
    colorModelSpec @LCH @Word "LCH"
