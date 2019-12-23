{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.CMYKSpec (spec) where

import Graphics.Color.Model.Common
import Graphics.Color.Model.CMYK
import Graphics.Color.Model.RGB
import Graphics.Color.Model.RGBSpec ()

instance (Elevator e, Random e) => Arbitrary (Color CMYK e) where
  arbitrary =
    ColorCMYK <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator <*>
    arbitraryElevator


spec :: Spec
spec =
  describe "CMYK" $ do
    colorModelSpec @CMYK @Word
    prop "rgb2cmyk . cmyk2rgb" $ \(rgb :: Color RGB Double) ->
      rgb `epsilonEqColor` cmyk2rgb (rgb2cmyk rgb)
