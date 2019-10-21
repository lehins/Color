{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorModel.CMYKSpec (spec) where

import Graphics.ColorModel.Common
import Graphics.ColorModel.CMYK
import Graphics.ColorModel.RGBSpec ()

instance (Elevator e, Random e) => Arbitrary (Pixel CMYK e) where
  arbitrary =
    PixelCMYK <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator <*>
    arbitraryElevator


spec :: Spec
spec =
  describe "CMYK" $ do
    colorModelSpec @CMYK @Word
    it "rgb2cmyk . cmyk2rgb" $ property $ \rgb -> rgb `epsilonEqPixel` cmyk2rgb (rgb2cmyk rgb)
