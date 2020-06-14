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
-- Module      : Graphics.Color.Space.RGB.Alternative.CMYK
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.Alternative.CMYK
  ( pattern ColorCMYK
  , pattern ColorCMYKA
  , CMYK
  , Color(CMYK)
  , module Graphics.Color.Space
  ) where

import Data.Coerce
import Data.Proxy
import Data.Typeable
import Foreign.Storable
import qualified Graphics.Color.Model.CMYK as CM
import Graphics.Color.Model.Internal
import Graphics.Color.Space
import Graphics.Color.Space.RGB.AdobeRGB as AdobeRGB
import qualified Graphics.Color.Space.RGB.ITU.Rec470 as Rec470
import Graphics.Color.Space.RGB.ITU.Rec601
import Graphics.Color.Space.RGB.ITU.Rec709
import Graphics.Color.Space.RGB.SRGB


-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
data CMYK cs

-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
newtype instance Color (CMYK cs) e = CMYK (Color CM.CMYK e)

-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Eq e => Eq (Color (CMYK cs) e)
-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Ord e => Ord (Color (CMYK cs) e)
-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Functor (Color (CMYK cs))
-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Applicative (Color (CMYK cs))
-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Foldable (Color (CMYK cs))
-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Traversable (Color (CMYK cs))
-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
deriving instance Storable e => Storable (Color (CMYK cs) e)

-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => Show (Color (CMYK cs) e) where
  showsPrec _ = showsColorModel

-- | Constructor for an RGB color space in an alternative CMYK color model
pattern ColorCMYK :: e -> e -> e -> e -> Color (CMYK cs) e
pattern ColorCMYK c m y k = CMYK (CM.ColorCMYK c m y k)
{-# COMPLETE ColorCMYK #-}

-- | Constructor for @CMYK@ with alpha channel.
pattern ColorCMYKA :: e -> e -> e -> e -> e -> Color (Alpha (CMYK cs)) e
pattern ColorCMYKA c m y k a = Alpha (CMYK (CM.ColorCMYK c m y k)) a
{-# COMPLETE ColorCMYKA #-}

-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
instance ColorModel cs e => ColorModel (CMYK cs) e where
  type Components (CMYK cs) e = (e, e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ = ("CMYK-" ++) . showsColorModelName (Proxy :: Proxy (Color cs e))


-- | `CMYK` representation for some (@`RedGreenBlue` cs i@) color space
instance (Typeable cs, ColorSpace (cs i l) i e, RedGreenBlue (cs i) i) =>
         ColorSpace (CMYK (cs i l)) i e where
  type BaseModel (CMYK (cs i l)) = CM.CMYK
  type BaseSpace (CMYK (cs i l)) = cs i l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}



-- | `CMYK` representation for `SRGB` color space
instance ColorSpace (SRGB l) D65 e => ColorSpace (CMYK (SRGB l)) D65 e where
  type BaseModel (CMYK (SRGB l)) = CM.CMYK
  type BaseSpace (CMYK (SRGB l)) = SRGB l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}

-- | `CMYK` representation for `AdobeRGB` color space
instance ColorSpace (AdobeRGB l) D65 e => ColorSpace (CMYK (AdobeRGB l)) D65 e where
  type BaseModel (CMYK (AdobeRGB l)) = CM.CMYK
  type BaseSpace (CMYK (AdobeRGB l)) = AdobeRGB l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}

-- | `CMYK` representation for `Rec470.BT470_525` color space
instance ColorSpace (Rec470.BT470_525 l) D65 e => ColorSpace (CMYK (Rec470.BT470_525 l)) D65 e where
  type BaseModel (CMYK (Rec470.BT470_525 l)) = CM.CMYK
  type BaseSpace (CMYK (Rec470.BT470_525 l)) = Rec470.BT470_525 l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}

-- | `CMYK` representation for `Rec470.BT470_625` color space
instance ColorSpace (Rec470.BT470_625 l) D65 e => ColorSpace (CMYK (Rec470.BT470_625 l)) D65 e where
  type BaseModel (CMYK (Rec470.BT470_625 l)) = CM.CMYK
  type BaseSpace (CMYK (Rec470.BT470_625 l)) = Rec470.BT470_625 l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}

-- | `CMYK` representation for `BT601_525` color space
instance ColorSpace (BT601_525 l) D65 e => ColorSpace (CMYK (BT601_525 l)) D65 e where
  type BaseModel (CMYK (BT601_525 l)) = CM.CMYK
  type BaseSpace (CMYK (BT601_525 l)) = BT601_525 l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}

-- | `CMYK` representation for `BT601_625` color space
instance ColorSpace (BT601_625 l) D65 e => ColorSpace (CMYK (BT601_625 l)) D65 e where
  type BaseModel (CMYK (BT601_625 l)) = CM.CMYK
  type BaseSpace (CMYK (BT601_625 l)) = BT601_625 l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}

-- | `CMYK` representation for `BT709` color space
instance ColorSpace (BT709 l) D65 e => ColorSpace (CMYK (BT709 l)) D65 e where
  type BaseModel (CMYK (BT709 l)) = CM.CMYK
  type BaseSpace (CMYK (BT709 l)) = BT709 l
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}
