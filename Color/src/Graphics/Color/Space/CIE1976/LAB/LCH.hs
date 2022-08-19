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
-- Module: Graphics.Color.Space.CIE1976.LAB.LCH

module Graphics.Color.Space.CIE1976.LAB.LCH
  ( pattern ColorLCHab
  , pattern ColorLCHabA
  , LCHab
  , Color(LCHab)
  ) where

import Data.List.NonEmpty
import Data.Coerce
import Data.Proxy
import Foreign.Storable
import qualified Graphics.Color.Model.LCH as CM
import Graphics.Color.Space.CIE1976.LAB
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal

-- | [CIE L*C*Hab](https://en.wikipedia.org/wiki/CIELAB_color_space),
--   an LCH representation for the L*a*b* color space
data LCHab (i :: k)

-- | Color in CIE L*C*Hab color space
newtype instance Color (LCHab i) e = LCHab (Color CM.LCH e)

-- | CIE1976 `LCHab` color space
deriving instance Eq e => Eq (Color (LCHab i) e)

-- | CIE1976 `LCHab` color space
deriving instance Ord e => Ord (Color (LCHab i) e)

-- | CIE1976 `LCHab` color space
deriving instance Functor (Color (LCHab i))

-- | CIE1976 `LCHab` color space
deriving instance Applicative (Color (LCHab i))

-- | CIE1976 `LCHab` color space
deriving instance Foldable (Color (LCHab i))

-- | CIE1976 `LCHab` color space
deriving instance Traversable (Color (LCHab i))

-- | CIE1976 `LCHab` color space
deriving instance Storable e => Storable (Color (LCHab i) e)

-- | CIE1976 `LCHab` color space
instance (Illuminant i, Elevator e) => Show (Color (LCHab i) e) where
  showsPrec _ = showsColorModel

-- | Constructor for a CIEL*a*b* color space in a cylindrical L*C*h parameterization
pattern ColorLCHab :: e -> e -> e -> Color (LCHab i) e
pattern ColorLCHab l c h = LCHab (CM.ColorLCH l c h)
{-# COMPLETE ColorLCHab #-}

-- | Constructor for a @LCHab@ with alpha
pattern ColorLCHabA :: e -> e -> e -> e -> Color (Alpha (LCHab i)) e
pattern ColorLCHabA l c h a = Alpha (LCHab (CM.ColorLCH l c h)) a
{-# COMPLETE ColorLCHabA #-}

-- | CIE1976 `LCHab` color space
instance (Illuminant i, Elevator e, ColorModel (LAB i) e) => ColorModel (LCHab i) e where
  type Components (LCHab i) e = (e, e, e)
  type ChannelCount (LCHab i) = 3
  channelCount _ = 3
  {-# INLINE channelCount #-}
  channelNames _ = "L" :| ["Cab", "Hab"]
  channelColors _ = V3 0x80 0x80 0x80 :|
                  [ V3 0x99 0xff 0x99
                  , V3 0x66 0x66 0xff
                  ]
  toComponents (LCHab lch) = toComponents lch
  {-# INLINE toComponents #-}
  fromComponents = LCHab . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ =
    ("LCH-"++) . showsColorModelName (Proxy :: Proxy (Color (LAB i) e))

instance (Illuminant i, Elevator e, ColorSpace (LAB i) i e) => ColorSpace (LCHab i) i e where
  type BaseModel (LCHab i) = CM.LCH
  type BaseSpace (LCHab i) = LAB i
  toBaseSpace (LCHab lch) = fmap fromDouble . fromComponents . CM.lch2lxy . fmap toDouble $ lch
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = LCHab . fmap fromDouble . CM.lxy2lch . toComponents . fmap toDouble
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}
  grayscale (coerce -> V3 l _ _) = X l
  {-# INLINE grayscale #-}
  replaceGrayscale (coerce -> V3 _ c h) (X l) = coerce (V3 l c h)
  {-# INLINE replaceGrayscale #-}
