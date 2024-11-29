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
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: Graphics.Color.Space.CIE1976.LUV.LCH

module Graphics.Color.Space.CIE1976.LUV.LCH
  ( pattern ColorLCHuv
  , pattern ColorLCHuvA
  , LCHuv
  , Color(LCHuv)
  ) where

import Data.List.NonEmpty
import Data.Coerce
import Data.Proxy
import Foreign.Storable
import qualified Graphics.Color.Model.LCH as CM
import Graphics.Color.Space.CIE1976.LUV
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal

-- | [CIE L*C*Huv](https://en.wikipedia.org/wiki/CIELUV_color_space),
--   an LCH representation for the L*u*v* color space
data LCHuv (i :: k)

-- | Color in CIE L*C*Huv color space
newtype instance Color (LCHuv i) e = LCHuv (Color CM.LCH e)

-- | CIE1976 `LCHuv` color space
deriving instance Eq e => Eq (Color (LCHuv i) e)

-- | CIE1976 `LCHuv` color space
deriving instance Ord e => Ord (Color (LCHuv i) e)

-- | CIE1976 `LCHuv` color space
deriving instance Functor (Color (LCHuv i))

-- | CIE1976 `LCHuv` color space
deriving instance Applicative (Color (LCHuv i))

-- | CIE1976 `LCHuv` color space
deriving instance Foldable (Color (LCHuv i))

-- | CIE1976 `LCHuv` color space
deriving instance Traversable (Color (LCHuv i))

-- | CIE1976 `LCHuv` color space
deriving instance Storable e => Storable (Color (LCHuv i) e)

-- | CIE1976 `LCHuv` color space
instance (Illuminant i, Elevator e) => Show (Color (LCHuv i) e) where
  showsPrec _ = showsColorModel

-- | Constructor for a CIEL*u*v* color space in a cylindrical L*C*h parameterization
pattern ColorLCHuv :: e -> e -> e -> Color (LCHuv i) e
pattern ColorLCHuv l c h = LCHuv (CM.ColorLCH l c h)
{-# COMPLETE ColorLCHuv #-}

-- | Constructor for a @LCHuv@ with alpha
pattern ColorLCHuvA :: e -> e -> e -> e -> Color (Alpha (LCHuv i)) e
pattern ColorLCHuvA l c h a = Alpha (LCHuv (CM.ColorLCH l c h)) a
{-# COMPLETE ColorLCHuvA #-}

-- | CIE1976 `LCHuv` color space
instance (Illuminant i, Elevator e, ColorModel (LUV i) e) => ColorModel (LCHuv i) e where
  type Components (LCHuv i) e = (e, e, e)
  type ChannelCount (LCHuv i) = 3
  channelCount _ = 3
  {-# INLINE channelCount #-}
  channelNames _ = "L" :| ["Cuv", "Huv"]
  channelColors _ = V3 0x80 0x80 0x80 :|
                  [ V3 0x99 0x00 0x99
                  , V3 0x99 0xcc 0x00
                  ]
  toComponents (LCHuv lch) =  toComponents lch
  {-# INLINE toComponents #-}
  fromComponents = LCHuv . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ =
    ("LCH-"++) . showsColorModelName (Proxy :: Proxy (Color (LUV i) e))

instance (Illuminant i, Elevator e, ColorSpace (LUV i) i e) => ColorSpace (LCHuv i) i e where
  type BaseModel (LCHuv i) = CM.LCH
  type BaseSpace (LCHuv i) = LUV i
  toBaseSpace (LCHuv lch) = fmap fromDouble . fromComponents . CM.lch2lxy . fmap toDouble $ lch
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = LCHuv . fmap fromDouble . CM.lxy2lch . toComponents . fmap toDouble
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}
  grayscale (coerce -> V3 l _ _) = X l
  {-# INLINE grayscale #-}
  replaceGrayscale (coerce -> V3 _ c h) (X l) = coerce (V3 l c h)
  {-# INLINE replaceGrayscale #-}
