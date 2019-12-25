{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.Color.Pixel
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Pixel
  ( Pixel(..)
  , convertPixel
  -- * sRGB color space
  , SRGB
  , D65
  , pattern PixelSRGB
  , pattern PixelSRGBA
  -- * Any RGB color space
  , AdobeRGB
  , pattern PixelRGB
  , pattern PixelRGBA
  , pattern PixelHSI
  , pattern PixelHSIA
  , pattern PixelYCbCr
  , pattern PixelYCbCrA
  , module Graphics.Color.Model
  , module Graphics.Color.Space
  ) where
import Data.Coerce
import Graphics.Color.Adaptation.VonKries
import Graphics.Color.Model
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.SRGB
import Graphics.Color.Space.RGB.AdobeRGB
import Graphics.Color.Space.RGB.Alternative
import Foreign.Storable

-- | Imaging is one of the most common places for a color to be used in.  where each pixel
-- has a specific color. This is a zero cost newtype wrapper around a `Color`.
--
-- @since 0.1.0
newtype Pixel cs e = Pixel
  { pixelColor :: Color cs e
  }

deriving instance Eq (Color cs e) => Eq (Pixel cs e)
deriving instance Ord (Color cs e) => Ord (Pixel cs e)
deriving instance Functor (Color cs) => Functor (Pixel cs)
deriving instance Applicative (Color cs) => Applicative (Pixel cs)
deriving instance Foldable (Color cs) => Foldable (Pixel cs)
deriving instance Traversable (Color cs) => Traversable (Pixel cs)
deriving instance Storable (Color cs e) => Storable (Pixel cs e)
instance Show (Color cs e) => Show (Pixel cs e) where
  show = show . pixelColor


-- | Convert a pixel from one color space to any other.
--
-- @since 0.1.0
convertPixel :: (ColorSpace cs' i' e', ColorSpace cs i e) => Pixel cs' e' -> Pixel cs e
convertPixel = Pixel . convert . pixelColor
{-# INLINE convertPixel #-}


-- | Constructor for a pixel in @sRGB@ color space
pattern PixelSRGB :: e -> e -> e -> Pixel SRGB e
pattern PixelSRGB r g b = Pixel (SRGB (CM.ColorRGB r g b))
{-# COMPLETE PixelSRGB #-}

-- | Constructor for a pixel in @sRGB@ color space with Alpha channel
pattern PixelSRGBA :: e -> e -> e -> e -> Pixel (Alpha SRGB) e
pattern PixelSRGBA r g b a = Pixel (Alpha (SRGB (CM.ColorRGB r g b)) a)
{-# COMPLETE PixelSRGBA #-}


-- | Constructor for a pixel in RGB color space.
pattern PixelRGB :: RedGreenBlue cs (i :: k) => e -> e -> e -> Pixel cs e
pattern PixelRGB r g b <- (coerce . unColorRGB . coerce -> V3 r g b) where
        PixelRGB r g b = coerce (mkColorRGB (coerce (V3 r g b)))
{-# COMPLETE PixelRGB #-}

-- -- | Constructor for a pixel in RGB color space.
-- pattern PixelRGB :: RedGreenBlue cs i => e -> e -> e -> Pixel cs e
-- pattern PixelRGB r g b <- (unColorRGB . pixelColor -> CM.ColorRGB r g b) where
--         PixelRGB r g b = Pixel (mkColorRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB #-}

-- | Constructor for a pixel in RGB color space with Alpha channel
pattern PixelRGBA :: RedGreenBlue cs i => e -> e -> e -> e -> Pixel (Alpha cs) e
pattern PixelRGBA r g b a <- (pixelColor -> Alpha (unColorRGB -> CM.ColorRGB r g b) a) where
        PixelRGBA r g b a = Pixel (Alpha (mkColorRGB (CM.ColorRGB r g b)) a)
{-# COMPLETE PixelRGBA #-}

-- | Constructor for @HSI@.
pattern PixelHSI :: e -> e -> e -> Pixel (HSI cs) e
pattern PixelHSI h s i = Pixel (ColorHSI h s i)
{-# COMPLETE PixelHSI #-}


-- | Constructor for @HSI@ with alpha channel.
pattern PixelHSIA :: e -> e -> e -> e -> Pixel (Alpha (HSI cs)) e
pattern PixelHSIA h s i a = Pixel (ColorHSIA h s i a)
{-# COMPLETE PixelHSIA #-}


-- | Constructor for @YCbCr@.
pattern PixelYCbCr :: e -> e -> e -> Pixel (YCbCr cs) e
pattern PixelYCbCr y cb cr = Pixel (ColorYCbCr y cb cr)
{-# COMPLETE PixelYCbCr #-}


-- | Constructor for @YCbCr@ with alpha channel.
pattern PixelYCbCrA :: e -> e -> e -> e -> Pixel (Alpha (YCbCr cs)) e
pattern PixelYCbCrA y cb cr a = Pixel (ColorYCbCrA y cb cr a)
{-# COMPLETE PixelYCbCrA #-}

toPixelY :: ColorSpace cs i e => Pixel cs e -> Pixel (Y i) e
toPixelY = Pixel . fmap (fromRealFloat @_ @Double) . toColorY . pixelColor

-- toPixelY :: (Elevator a, RealFloat a, ColorSpace cs i e) => Pixel cs e -> Pixel Y a
-- toPixelY = Pixel . toColorY . pixelColor

-- -- | Constructor for a pixel in @sRGB@ color space with 8-bits per channel
-- pattern PixelRGB8 :: Word8 -> Word8 -> Word8 -> Pixel SRGB Word8
-- pattern PixelRGB8 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB8 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 16-bits per channel
-- pattern PixelRGB16 :: Word16 -> Word16 -> Word16 -> Pixel SRGB Word16
-- pattern PixelRGB16 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB16 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 32-bits per channel
-- pattern PixelRGB32 :: Word32 -> Word32 -> Word32 -> Pixel SRGB Word32
-- pattern PixelRGB32 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB32 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 64-bits per channel
-- pattern PixelRGB64 :: Word64 -> Word64 -> Word64 -> Pixel SRGB Word64
-- pattern PixelRGB64 r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGB64 #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 32-bit floating point value per channel
-- pattern PixelRGBF :: Float -> Float -> Float -> Pixel SRGB Float
-- pattern PixelRGBF r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGBF #-}

-- -- | Constructor for a pixel in @sRGB@ color space with 32-bit floating point value per channel
-- pattern PixelRGBD :: Double -> Double -> Double -> Pixel SRGB Double
-- pattern PixelRGBD r g b = Pixel (SRGB (CM.ColorRGB r g b))
-- {-# COMPLETE PixelRGBD #-}
