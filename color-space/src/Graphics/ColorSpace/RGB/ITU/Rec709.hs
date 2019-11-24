{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.ColorSpace.RGB.ITU.Rec709
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.RGB.ITU.Rec709
  ( BT709
  , D65
  , primaries
  , Rec601.transfer
  , Rec601.itransfer
  , module Graphics.ColorSpace
  ) where

import Foreign.Storable
import Graphics.ColorModel.Internal
import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorSpace
import Graphics.ColorSpace.RGB.ITU.Rec601 as Rec601 (D65, itransfer, transfer)
import Graphics.ColorSpace.RGB.Luma

-- | ITU-R BT.709 color space
data BT709

newtype instance Pixel BT709 e = BT709 (Pixel CM.RGB e)

-- | ITU-R BT.709 color space
deriving instance Eq e => Eq (Pixel BT709 e)
-- | ITU-R BT.709 color space
deriving instance Ord e => Ord (Pixel BT709 e)
-- | ITU-R BT.709 color space
deriving instance Functor (Pixel BT709)
-- | ITU-R BT.709 color space
deriving instance Applicative (Pixel BT709)
-- | ITU-R BT.709 color space
deriving instance Foldable (Pixel BT709)
-- | ITU-R BT.709 color space
deriving instance Traversable (Pixel BT709)
-- | ITU-R BT.709 color space
deriving instance Storable e => Storable (Pixel BT709 e)

-- | ITU-R BT.709 color space
instance Elevator e => Show (Pixel BT709 e) where
  showsPrec _ = showsColorModel

-- | ITU-R BT.709 color space
instance Elevator e => ColorModel BT709 e where
  type Components BT709 e = (e, e, e)
  toComponents = toComponents . unPixelRGB
  {-# INLINE toComponents #-}
  fromComponents = mkPixelRGB . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName = showsColorModelName . unPixelRGB

-- | ITU-R BT.709 color space
instance Elevator e => ColorSpace BT709 e where
  type BaseColorSpace BT709 = BT709
  toBaseColorSpace = id
  {-# INLINE toBaseColorSpace #-}
  fromBaseColorSpace = id
  {-# INLINE fromBaseColorSpace #-}
  toPixelXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toPixelXYZ #-}
  fromPixelXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromPixelXYZ #-}
  showsColorSpaceName _ = ("sBT709 Standard" ++)

-- | ITU-R BT.709 color space
instance RedGreenBlue BT709 D65 where
  chromaticity = primaries
  ecctf = fmap Rec601.transfer
  {-# INLINE ecctf #-}
  dcctf = fmap Rec601.itransfer
  {-# INLINE dcctf #-}


-- | Primaries for ITU-R BT.709, which are also the primaries for sRGB color space.
--
-- @since 0.1.0
primaries :: (Illuminant i, RealFloat e) => Chromaticity rgb i e
primaries = Chromaticity (Primary 0.64 0.33)
                         (Primary 0.30 0.60)
                         (Primary 0.15 0.06)


instance Luma BT709 where
  rWeight = 0.2126
  gWeight = 0.7152
  bWeight = 0.0722
