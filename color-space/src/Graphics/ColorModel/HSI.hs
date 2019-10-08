{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorModel.HSI
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.HSI
  ( HSI
  , Pixel(..)
  , hsi2rgb
  , rgb2hsi
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal
import Graphics.ColorModel.RGB

-----------
--- HSI ---
-----------

-- | Hue, Saturation and Intensity color space.
data HSI

-- | `HSI` color model
data instance Pixel HSI e = PixelHSI !e !e !e
-- | `HSI` color model
deriving instance Eq e => Eq (Pixel HSI e)
-- | `HSI` color model
deriving instance Ord e => Ord (Pixel HSI e)

-- | `HSI` color model
instance Elevator e => Show (Pixel HSI e) where
  showsPrec _ = showsColorModel

-- | `HSI` color model
instance Elevator e => ColorModel HSI e where
  type Components HSI e = (e, e, e)
  toComponents (PixelHSI h s i) = (h, s, i)
  {-# INLINE toComponents #-}
  fromComponents (h, s, i) = PixelHSI h s i
  {-# INLINE fromComponents #-}

-- | `HSI` color model
instance Functor (Pixel HSI) where
  fmap f (PixelHSI h s i) = PixelHSI (f h) (f s) (f i)
  {-# INLINE fmap #-}

-- | `HSI` color model
instance Applicative (Pixel HSI) where
  pure !e = PixelHSI e e e
  {-# INLINE pure #-}
  (PixelHSI fh fs fi) <*> (PixelHSI h s i) = PixelHSI (fh h) (fs s) (fi i)
  {-# INLINE (<*>) #-}

-- | `HSI` color model
instance Foldable (Pixel HSI) where
  foldr f !z (PixelHSI h s i) = f h (f s (f i z))
  {-# INLINE foldr #-}

-- | `HSI` color model
instance Traversable (Pixel HSI) where
  traverse f (PixelHSI h s i) = PixelHSI <$> f h <*> f s <*> f i
  {-# INLINE traverse #-}

-- | `HSI` color model
instance Storable e => Storable (Pixel HSI e) where
  sizeOf = sizeOfN 3
  {-# INLINE sizeOf #-}
  alignment = alignmentN 3
  {-# INLINE alignment #-}
  peek = peek3 PixelHSI
  {-# INLINE peek #-}
  poke p (PixelHSI h s i) = poke3 p h s i
  {-# INLINE poke #-}

-- TODO: switch to Either
hsi2rgb :: Pixel HSI Double -> Pixel RGB Double
hsi2rgb (PixelHSI h' s i) = getRGB (h' * 2 * pi)
  where
    !is = i * s
    !second = i - is
    errorHue = error $ "HSI pixel is not properly scaled, Hue: " ++ show h'
    getFirst !a !b = i + is * cos a / cos b
    {-# INLINE getFirst #-}
    getThird !v1 !v2 = i + 2 * is + v1 - v2
    {-# INLINE getThird #-}
    getRGB h
      | h < 0 = errorHue
      | h < 2 * pi / 3 =
        let !r = getFirst h (pi / 3 - h)
            !b = second
            !g = getThird b r
         in PixelRGB r g b
      | h < 4 * pi / 3 =
        let !g = getFirst (h - 2 * pi / 3) (h + pi)
            !r = second
            !b = getThird r g
         in PixelRGB r g b
      | h < 2 * pi =
        let !b = getFirst (h - 4 * pi / 3) (2 * pi - pi / 3 - h)
            !g = second
            !r = getThird g b
         in PixelRGB r g b
      | otherwise = errorHue
    {-# INLINE getRGB #-}
{-# INLINE hsi2rgb #-}


rgb2hsi :: Pixel RGB Double -> Pixel HSI Double
rgb2hsi (PixelRGB r g b) = PixelHSI h s i
  where
    !h' = atan2 y x
    !h =
      (if h' < 0
         then h' + 2 * pi
         else h') /
      (2 * pi)
    !s =
      if i == 0
        then 0
        else 1 - minimum [r, g, b] / i
    !i = (r + g + b) / 3
    !x = (2 * r - g - b) / 2.449489742783178
    !y = (g - b) / 1.4142135623730951
{-# INLINE rgb2hsi #-}



-- ------------
-- --- HSIA ---
-- ------------

-- -- | Hue, Saturation and Intensity color space with Alpha channel.
-- data HSIA = HueHSIA   -- ^ Hue
--           | SatHSIA   -- ^ Saturation
--           | IntHSIA   -- ^ Intensity
--           | AlphaHSIA -- ^ Alpha
--           deriving (Eq, Enum, Show, Bounded, Typeable)


-- data instance Pixel HSIA e = PixelHSIA !e !e !e !e deriving (Eq, Ord)


-- instance Show e => Show (Pixel HSIA e) where
--   show (PixelHSIA h s i a) = "<HSIA:("++show h++"|"++show s++"|"++show i++"|"++show a++")>"


-- instance Elevator e => ColorModel HSIA e where
--   type Components HSIA e = (e, e, e, e)

--   toComponents (PixelHSIA h s i a) = (h, s, i, a)
--   {-# INLINE toComponents #-}
--   fromComponents (h, s, i, a) = PixelHSIA h s i a
--   {-# INLINE fromComponents #-}
--   getPxC (PixelHSIA h _ _ _) HueHSIA   = h
--   getPxC (PixelHSIA _ s _ _) SatHSIA   = s
--   getPxC (PixelHSIA _ _ i _) IntHSIA   = i
--   getPxC (PixelHSIA _ _ _ a) AlphaHSIA = a
--   {-# INLINE getPxC #-}
--   setPxC (PixelHSIA _ s i a) HueHSIA h   = PixelHSIA h s i a
--   setPxC (PixelHSIA h _ i a) SatHSIA s   = PixelHSIA h s i a
--   setPxC (PixelHSIA h s _ a) IntHSIA i   = PixelHSIA h s i a
--   setPxC (PixelHSIA h s i _) AlphaHSIA a = PixelHSIA h s i a
--   {-# INLINE setPxC #-}
--   mapPxC f (PixelHSIA h s i a) =
--     PixelHSIA (f HueHSIA h) (f SatHSIA s) (f IntHSIA i) (f AlphaHSIA a)
--   {-# INLINE mapPxC #-}


-- instance Elevator e => AlphaModel HSIA e where
--   type Opaque HSIA = HSI

--   getAlpha (PixelHSIA _ _ _ a) = a
--   {-# INLINE getAlpha #-}
--   addAlpha !a (PixelHSI h s i) = PixelHSIA h s i a
--   {-# INLINE addAlpha #-}
--   dropAlpha (PixelHSIA h s i _) = PixelHSI h s i
--   {-# INLINE dropAlpha #-}


-- instance Functor (Pixel HSIA) where
--   fmap f (PixelHSIA h s i a) = PixelHSIA (f h) (f s) (f i) (f a)
--   {-# INLINE fmap #-}


-- instance Applicative (Pixel HSIA) where
--   pure !e = PixelHSIA e e e e
--   {-# INLINE pure #-}
--   (PixelHSIA fh fs fi fa) <*> (PixelHSIA h s i a) = PixelHSIA (fh h) (fs s) (fi i) (fa a)
--   {-# INLINE (<*>) #-}


-- instance Foldable (Pixel HSIA) where
--   foldr f !z (PixelHSIA h s i a) = f h (f s (f i (f a z)))
--   {-# INLINE foldr #-}

-- instance Traversable (Pixel HSIA) where
--   traverse f (PixelHSIA h s i a) = PixelHSIA <$> f h <*> f s <*> f i <*> f a
--   {-# INLINE traverse #-}


-- instance Storable e => Storable (Pixel HSIA e) where
--   sizeOf _ = 4 * sizeOf (undefined :: e)
--   {-# INLINE sizeOf #-}
--   alignment _ = alignment (undefined :: e)
--   {-# INLINE alignment #-}
--   peek !p = do
--     let !q = castPtr p
--     h <- peek q
--     s <- peekElemOff q 1
--     i <- peekElemOff q 2
--     a <- peekElemOff q 3
--     return $! PixelHSIA h s i a
--   {-# INLINE peek #-}
--   poke !p (PixelHSIA h s i a) = do
--     let !q = castPtr p
--     poke q h
--     pokeElemOff q 1 s
--     pokeElemOff q 2 i
--     pokeElemOff q 3 a
--   {-# INLINE poke #-}
