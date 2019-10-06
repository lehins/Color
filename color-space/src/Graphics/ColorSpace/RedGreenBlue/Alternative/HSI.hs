{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
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
import Graphics.ColorModel.Helpers


data HSI (cs :: k -> *) (i :: k)

newtype instance Pixel (HSI cs i) e = HSI (Pixel CM.HSI e)
  deriving (Eq, Functor, Applicative, Foldable, Traversable, Storable)

instance Show e => Show (Pixel (HSI cs i) e) where
  showsPrec _ (PixelHSI r g b) =
    showsP "HSI:" -- ++ show i
    (shows3 r g b)


-- | Constructor for the most common @sRGB@ color space with the default `D65` illuminant
pattern PixelHSI :: e -> e -> e -> Pixel (HSI cs i) e
pattern PixelHSI h s i = HSI (CM.PixelHSI h s i)
{-# COMPLETE PixelHSI #-}

-- TODO:
pattern PixelHSI360 :: Double -> Double -> Double -> Pixel (HSI cs i) Double
pattern PixelHSI360 h s i <- HSI (CM.PixelHSI ((* 360) -> h) s i) where
        PixelHSI360 h s i = HSI (CM.PixelHSI (h / 360) s i)
{-# COMPLETE PixelHSI360 #-}



instance (Typeable i, Typeable k, Typeable cs, Elevator e) => ColorModel (HSI cs (i :: k)) e where
  type Components (HSI cs i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}


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
