{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.ITU.Rec470
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.ITU.Rec470
  ( pattern BT470_525
  , BT470_525
  , C
  , pattern BT470_625
  , BT470_625
  , D65
  , primaries525
  , primaries625
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Typeable
import Foreign.Storable
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Space
import Graphics.Color.Illuminant.ITU.Rec470

------------------------------------
-- ITU-R BT.470 (525) --------------
------------------------------------

-- | [ITU-R BT.470](https://www.itu.int/rec/R-REC-BT.470) (525) color space. Used in NTSC
data BT470_525 (l :: Linearity)

newtype instance Color (BT470_525 l) e = BT470_525 (Color CM.RGB e)

-- | ITU-R BT.470 (525) color space
deriving instance Eq e => Eq (Color (BT470_525 l) e)
-- | ITU-R BT.470 (525) color space
deriving instance Ord e => Ord (Color (BT470_525 l) e)
-- | ITU-R BT.470 (525) color space
deriving instance Functor (Color (BT470_525 l))
-- | ITU-R BT.470 (525) color space
deriving instance Applicative (Color (BT470_525 l))
-- | ITU-R BT.470 (525) color space
deriving instance Foldable (Color (BT470_525 l))
-- | ITU-R BT.470 (525) color space
deriving instance Traversable (Color (BT470_525 l))
-- | ITU-R BT.470 (525) color space
deriving instance Storable e => Storable (Color (BT470_525 l) e)

-- | ITU-R BT.470 (525) color space
instance (Typeable l, Elevator e) => Show (Color (BT470_525 l) e) where
  showsPrec _ = showsColorModel

-- | ITU-R BT.470 (525) color space
instance (Typeable l, Elevator e) => ColorModel (BT470_525 l) e where
  type Components (BT470_525 l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | ITU-R BT.470 (525) color space
instance Elevator e => ColorSpace (BT470_525 'Linear) C e where
  type BaseModel (BT470_525 'Linear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLinearLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgbLinear2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgbLinear
  {-# INLINE fromColorXYZ #-}

-- | ITU-R BT.470 (525) color space
instance Elevator e => ColorSpace (BT470_525 'NonLinear) C e where
  type BaseModel (BT470_525 'NonLinear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | ITU-R BT.470 (525) color space
instance RedGreenBlue BT470_525 C where
  gamut = primaries525
  ecctf c = BT470_525 (fmap (gamma 2.2) (coerce c))
  {-# INLINE ecctf #-}
  dcctf c = BT470_525 (fmap (igamma 2.2) (coerce c))
  {-# INLINE dcctf #-}

------------------------------------
-- ITU-R BT.470 (625) --------------
------------------------------------

-- | [ITU-R BT.470](https://www.itu.int/rec/R-REC-BT.470) (625) color space. Used in PAL/SECAM
data BT470_625 (l :: Linearity)

newtype instance Color (BT470_625 l) e = BT470_625 (Color CM.RGB e)

-- | ITU-R BT.470 (625) color space
deriving instance Eq e => Eq (Color (BT470_625 l) e)
-- | ITU-R BT.470 (625) color space
deriving instance Ord e => Ord (Color (BT470_625 l) e)
-- | ITU-R BT.470 (625) color space
deriving instance Functor (Color (BT470_625 l))
-- | ITU-R BT.470 (625) color space
deriving instance Applicative (Color (BT470_625 l))
-- | ITU-R BT.470 (625) color space
deriving instance Foldable (Color (BT470_625 l))
-- | ITU-R BT.470 (625) color space
deriving instance Traversable (Color (BT470_625 l))
-- | ITU-R BT.470 (625) color space
deriving instance Storable e => Storable (Color (BT470_625 l) e)

-- | ITU-R BT.470 (625) color space
instance (Typeable l, Elevator e) => Show (Color (BT470_625 l) e) where
  showsPrec _ = showsColorModel

-- | ITU-R BT.470 (625) color space
instance (Typeable l, Elevator e) => ColorModel (BT470_625 l) e where
  type Components (BT470_625 l) e = (e, e, e)
  toComponents = toComponents . unColorRGB
  {-# INLINE toComponents #-}
  fromComponents = mkColorRGB . fromComponents
  {-# INLINE fromComponents #-}

-- | ITU-R BT.470 (625) color space
instance Elevator e => ColorSpace (BT470_625 'Linear) D65 e where
  type BaseModel (BT470_625 'Linear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLinearLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgbLinear2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgbLinear
  {-# INLINE fromColorXYZ #-}

-- | ITU-R BT.470 (625) color space
instance Elevator e => ColorSpace (BT470_625 'NonLinear) D65 e where
  type BaseModel (BT470_625 'NonLinear) = CM.RGB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = rgbLuminance . fmap toRealFloat
  {-# INLINE luminance #-}
  toColorXYZ = rgb2xyz . fmap toRealFloat
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fmap fromRealFloat . xyz2rgb
  {-# INLINE fromColorXYZ #-}

-- | ITU-R BT.470 (625) color space
instance RedGreenBlue BT470_625 D65 where
  gamut = primaries625
  ecctf c = BT470_625 (fmap (gamma 2.8) (coerce c))
  {-# INLINE ecctf #-}
  dcctf c = BT470_625 (fmap (igamma 2.8) (coerce c))
  {-# INLINE dcctf #-}

gamma :: Floating a => a -> a -> a
gamma p v = v ** p
{-# INLINE gamma #-}

igamma :: Floating a => a -> a -> a
igamma p v = v ** (1 / p)
{-# INLINE igamma #-}

-- | Primaries for ITU-R BT.470 (525).
--
-- @since 0.1.0
primaries525 :: RealFloat e => Gamut rgb i e
primaries525 = Gamut (Primary 0.67 0.33)
                     (Primary 0.21 0.71)
                     (Primary 0.14 0.08)


-- | Primaries for ITU-R BT.470 and BT.601 (625).
--
-- @since 0.1.0
primaries625 :: RealFloat e => Gamut rgb i e
primaries625 = Gamut (Primary 0.64 0.33)
                     (Primary 0.29 0.60)
                     (Primary 0.15 0.06)

