{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Graphics.ColorSpace.RedGreenBlue.Alternative.HSI
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RedGreenBlue.HSI
  ( pattern PixelHSI
  , pattern PixelHSI360
  , HSI
  , Pixel(HSI)
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import Graphics.ColorModel.Internal
import qualified Graphics.ColorModel.HSI as CM
import Graphics.ColorSpace.Internal
import Graphics.ColorSpace.RedGreenBlue.Internal

data HSI (cs :: k -> *) (i :: k)

newtype instance Pixel (HSI cs i) e = HSI (Pixel CM.HSI e)
  deriving (Eq, Functor, Applicative, Foldable, Traversable, Storable)

instance ColorModel (cs i) e => Show (Pixel (HSI cs i) e) where
  showsPrec _ px@(PixelHSI h s i) = showsP (showsColorModel px) (shows3 h s i)

-- | Constructor for the most common @sRGB@ color space with the default `D65` illuminant
pattern PixelHSI :: e -> e -> e -> Pixel (HSI cs i) e
pattern PixelHSI h s i = HSI (CM.PixelHSI h s i)
{-# COMPLETE PixelHSI #-}

-- TODO: doc, warning about outside of 0-360.
pattern PixelHSI360 :: Double -> Double -> Double -> Pixel (HSI cs i) Double
pattern PixelHSI360 h s i <- HSI (CM.PixelHSI ((* 360) -> h) s i) where
        PixelHSI360 h s i = HSI (CM.PixelHSI (h / 360) s i)
{-# COMPLETE PixelHSI360 #-}



instance ColorModel (cs i) e => ColorModel (HSI cs (i :: k)) e where
  type Components (HSI cs i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModel _ = ("HSI (" ++) . showsColorModel (pure 0 :: Pixel (cs i) e) . (")" ++)


-- | HSI color space, that is based on some actual RGB color space.
instance (Typeable cs, Typeable k, Typeable i, ColorSpace (cs i) e, RedGreenBlue cs i) =>
         ColorSpace (HSI cs (i :: k)) e where
  type BaseColorSpace (HSI cs i) = cs i
  toBaseColorSpace = mkPixelRGB . fmap fromDouble . CM.hsi2rgb . fmap toDouble . coerce
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = coerce . fmap fromDouble . CM.rgb2hsi . fmap toDouble . unPixelRGB
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = toPixelXYZ . toBaseColorSpace
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fromBaseColorSpace . fromPixelXYZ
  {-# INLINE fromPixelXYZ #-}
