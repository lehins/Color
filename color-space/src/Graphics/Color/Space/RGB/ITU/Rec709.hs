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
-- Module      : Graphics.Color.Space.RGB.ITU.Rec709
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.ITU.Rec709
  ( BT709
  , D65
  , primaries
  , Rec601.transfer
  , Rec601.itransfer
  , module Graphics.Color.Space
  ) where

import Foreign.Storable
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.ITU.Rec601 as Rec601 (D65, itransfer, transfer)
import Graphics.Color.Space.RGB.Luma

-- | ITU-R BT.709 color space
data BT709

newtype instance Color BT709 e = BT709 (Color CM.RGB e)

-- | ITU-R BT.709 color space
deriving instance Eq e => Eq (Color BT709 e)
-- | ITU-R BT.709 color space
deriving instance Ord e => Ord (Color BT709 e)
-- | ITU-R BT.709 color space
deriving instance Functor (Color BT709)
-- | ITU-R BT.709 color space
deriving instance Applicative (Color BT709)
-- | ITU-R BT.709 color space
deriving instance Foldable (Color BT709)
-- | ITU-R BT.709 color space
deriving instance Traversable (Color BT709)
-- | ITU-R BT.709 color space
deriving instance Storable e => Storable (Color BT709 e)

-- | ITU-R BT.709 color space
instance Elevator e => Show (Color BT709 e) where
  showsPrec _ = showsColorModel

-- | ITU-R BT.709 color space
instance Elevator e => ColorModel BT709 e where
  type Components BT709 e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | ITU-R BT.709 color space
instance Elevator e => ColorSpace BT709 D65 e where
  type BaseModel BT709 = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | ITU-R BT.709 color space
instance RedGreenBlue BT709 D65 where
  gamut = primaries
  ecctf = fmap Rec601.transfer
  {-# INLINE ecctf #-}
  dcctf = fmap Rec601.itransfer
  {-# INLINE dcctf #-}


-- | Primaries for ITU-R BT.709, which are also the primaries for sRGB color space.
--
-- @since 0.1.0
primaries :: RealFloat e => Gamut rgb i e
primaries = Gamut (Primary 0.64 0.33)
                  (Primary 0.30 0.60)
                  (Primary 0.15 0.06)


instance Luma BT709 where
  rWeight = 0.2126
  gWeight = 0.7152
  bWeight = 0.0722
