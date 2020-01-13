{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.YCbCrSpec (spec) where

import Graphics.Color.Model
import Graphics.Color.Model.Common
import Graphics.Color.Model.RGBSpec ()

instance (Elevator e, Random e) => Arbitrary (Color YCbCr e) where
  arbitrary =
    ColorYCbCr <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec =
  describe "YCbCr" $
    colorModelSpec @YCbCr @Word "YCbCr"
