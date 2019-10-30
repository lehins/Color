-- |
-- Module      : Graphics.ColorModel
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel
  ( ColorModel(..)
  -- , ToRGB(..)
  , Elevator(..)
  , Pixel
  ) where

import Graphics.ColorModel.Alpha
import Graphics.ColorModel.HSI
import Graphics.ColorModel.HSL
import Graphics.ColorModel.HSV
import Graphics.ColorModel.Internal
import Graphics.ColorModel.RGB
import Graphics.ColorModel.CMYK


-- -- | Conversion to `RGB` color model.
-- class ToRGB cs where

--   -- | Convert to an `RGB` pixel.
--   toPixelRGB :: (Elevator e, Elevator a, RealFloat a) => Pixel cs e -> Pixel RGB a
--   -- | Convert to an `RGB` pixel with alpha channel
--   toPixelRGBA :: (Elevator e, Elevator a, RealFloat a) => Pixel cs e -> Pixel (Alpha RGB) a
--   toPixelRGBA = (`addAlpha` 1) . toPixelRGB
--   {-# INLINE toPixelRGBA #-}

-- instance ToRGB cs => ToRGB (Alpha cs) where
--   toPixelRGB = toPixelRGB . dropAlpha
--   {-# INLINE toPixelRGB #-}
--   toPixelRGBA (Alpha px a) = Alpha (toPixelRGB px) (toRealFloat a)
--   {-# INLINE toPixelRGBA #-}

-- instance ToRGB RGB where
--   toPixelRGB = fmap toRealFloat
--   {-# INLINE toPixelRGB #-}

-- instance ToRGB CMYK where
--   toPixelRGB = cmyk2rgb . fmap toRealFloat
--   {-# INLINE toPixelRGB #-}

-- instance ToRGB HSI where
--   toPixelRGB = hsi2rgb . fmap toRealFloat
--   {-# INLINE toPixelRGB #-}

-- instance ToRGB HSL where
--   toPixelRGB = hsl2rgb . fmap toRealFloat
--   {-# INLINE toPixelRGB #-}

-- instance ToRGB HSV where
--   toPixelRGB = hsv2rgb . fmap toRealFloat
--   {-# INLINE toPixelRGB #-}
