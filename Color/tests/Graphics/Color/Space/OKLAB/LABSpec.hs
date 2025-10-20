{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.OKLAB.LABSpec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Space.RGB.Derived.SRGBSpec ()
import Graphics.Color.Space.OKLAB

instance (Elevator e, Random e) => Arbitrary (Color OKLAB e) where
  arbitrary = ColorOKLAB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator

spec :: Spec
spec = describe "OKLAB" $ do
  colorModelSpec @OKLAB @Word "OKLAB"
  colorSpaceLenientSpec @OKLAB @Double 1e-9
