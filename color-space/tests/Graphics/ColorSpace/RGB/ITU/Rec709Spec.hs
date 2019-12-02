{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorSpace.RGB.ITU.Rec709Spec (spec) where

import Graphics.ColorSpace.Common
import Graphics.ColorSpace.RGB.ITU.Rec709

instance (Elevator e, Random e) => Arbitrary (Pixel BT709 e) where
  arbitrary = PixelRGB <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


spec :: Spec
spec = describe "Rec709" $ do
  colorModelSpec @BT709 @Word
  -- Roundrtrip is not always very accurate, eg: 8.115324539550295e-2 /= 8.140132075907752e-2
  prop "toFromPixelXYZ (lenient)" $
    prop_toFromLenientPixelXYZ @BT709 @_ @Double 5e-4
  prop "toFromColorSpace" $ prop_toFromColorSpace @BT709 @_ @Double
