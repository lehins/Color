{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Graphics.Color.Space.RGB.Alternative.YCbCr
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Alternative.YCbCr
  ( pattern ColorYCbCr
  , pattern ColorYCbCrA
  , YCbCr
  , Color(YCbCr)
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Proxy
import Foreign.Storable
import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
import qualified Graphics.Color.Model.YCbCr as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.SRGB
import Graphics.Color.Space.RGB.Luma

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
data YCbCr cs

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Color (YCbCr cs) e = YCbCr (Color CM.YCbCr e)

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Color (YCbCr cs) e)
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Color (YCbCr cs) e)
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Color (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Color (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Color (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Color (YCbCr cs))
-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Color (YCbCr cs) e)

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Color (YCbCr cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative YCbCr color model
pattern ColorYCbCr :: e -> e -> e -> Color (YCbCr cs) e
pattern ColorYCbCr y cb cr = YCbCr (CM.ColorYCbCr y cb cr)
{-# COMPLETE ColorYCbCr #-}

-- | Constructor for @YCbCr@ with alpha channel.
pattern ColorYCbCrA :: e -> e -> e -> e -> Color (Alpha (YCbCr cs)) e
pattern ColorYCbCrA y cb cr a = Alpha (YCbCr (CM.ColorYCbCr y cb cr)) a
{-# COMPLETE ColorYCbCrA #-}


-- | `YCbCr` color model
instance ColorModel cs e => ColorModel (YCbCr cs) e where
  type Components (YCbCr cs) e = (e, e, e)
  toComponents (ColorYCbCr y cb cr) = (y, cb, cr)
  {-# INLINE toComponents #-}
  fromComponents (y, cb, cr) = ColorYCbCr y cb cr
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("YCbCr-" ++) . showsColorModelName (Proxy :: Proxy (Color cs e))

-- | `YCbCr` representation for `SRGB` color space
instance Elevator e => ColorSpace (YCbCr SRGB) D65 e where
  type BaseModel (YCbCr SRGB) = CM.YCbCr
  type BaseSpace (YCbCr SRGB) = SRGB
  toBaseSpace = fmap fromRealFloat . ycbcr2rgb . fmap toFloat
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = fmap fromRealFloat . rgb2ycbcr . fmap toFloat
  {-# INLINE fromBaseSpace #-}
  toColorXYZ = toColorXYZ . toBaseSpace
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fromBaseSpace . fromColorXYZ
  {-# INLINE fromColorXYZ #-}

-- | `YCbCr` representation for some (@`RedGreenBlue` cs i@) color space
instance (Luma (cs i), ColorSpace (cs i) i e, RedGreenBlue (cs i) i) =>
         ColorSpace (YCbCr (cs i)) i e where
  type BaseModel (YCbCr (cs i)) = CM.YCbCr
  type BaseSpace (YCbCr (cs i)) = cs i
  toBaseSpace = fmap fromDouble . fromColorYCbCr
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = fmap fromDouble . toColorYCbCr
  {-# INLINE fromBaseSpace #-}
  toColorXYZ = toColorXYZ . toBaseSpace
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = fromBaseSpace . fromColorXYZ
  {-# INLINE fromColorXYZ #-}




-- | Source: ITU-T Rec. T.871
ycbcr2rgb :: (RedGreenBlue cs i, Ord e, Floating e) => Color (YCbCr cs) e -> Color cs e
ycbcr2rgb (ColorYCbCr y cb cr) = ColorRGB r g b
  where
    !cb05 = cb - 0.5
    !cr05 = cr - 0.5
    !r = clamp01 (y                   + 1.402    * cr05)
    !g = clamp01 (y - 0.344136 * cb05 - 0.714136 * cr05)
    !b = clamp01 (y + 1.772    * cb05)
{-# INLINE ycbcr2rgb #-}

-- | Source: ITU-T Rec. T.871
rgb2ycbcr :: (RedGreenBlue cs i, Floating e) => Color cs e -> Color (YCbCr cs) e
rgb2ycbcr (ColorRGB r g b) = ColorYCbCr y cb cr
  where
    !y  =          0.299 * r +    0.587 * g +    0.114 * b
    !cb = 0.5 - 0.168736 * r - 0.331264 * g +      0.5 * b
    !cr = 0.5 +      0.5 * r - 0.418688 * g - 0.081312 * b
{-# INLINE rgb2ycbcr #-}


toColorYCbCr ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Color cs e'
  -> Color (YCbCr cs) e
toColorYCbCr rgb = YCbCr (CM.rgb2ycbcr (unColorRGB rgb) weights)
  where
    !weights = rgbLumaWeights rgb
{-# INLINE toColorYCbCr #-}

fromColorYCbCr ::
     forall cs i e' e. (Luma cs, RedGreenBlue cs i, Elevator e', Elevator e, RealFloat e)
  => Color (YCbCr cs) e'
  -> Color cs e
fromColorYCbCr ycbcr = rgb
  where
    !rgb = mkColorRGB (CM.ycbcr2rgb (coerce ycbcr :: Color CM.YCbCr e') weights)
    !weights = rgbLumaWeights rgb
{-# INLINE fromColorYCbCr #-}
