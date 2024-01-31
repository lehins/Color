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
-- Module: Graphics.Color.Space.OKLAB.LCH

module Graphics.Color.Space.OKLAB.LCH
  ( pattern ColorOKLCH
  , pattern ColorOKLCHA
  , OKLCH
  , Color(OKLCH)
  ) where

import Data.Coerce
import Data.List.NonEmpty
import Data.Proxy
import Foreign.Storable
import qualified Graphics.Color.Model.LCH as CM
import Graphics.Color.Space.OKLAB
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal
import Graphics.Color.Illuminant.ITU.Rec601 (D65)

-- | [OK L*C*H*](https://bottosson.github.io/posts/oklab/) color space
--   an LCH representation for the L*a*b* color space
data OKLCH

-- | Color in OKLCH color space
newtype instance Color OKLCH e = OKLCH (Color CM.LCH e)

-- | `OKLCH` color space
deriving instance Eq e => Eq (Color OKLCH e)

-- | `OKLCH` color space
deriving instance Ord e => Ord (Color OKLCH e)

-- | `OKLCH` color space
deriving instance Functor (Color OKLCH)

-- | `OKLCH` color space
deriving instance Applicative (Color OKLCH)

-- | `OKLCH` color space
deriving instance Foldable (Color OKLCH)

-- | `OKLCH` color space
deriving instance Traversable (Color OKLCH)

-- | `OKLCH` color space
deriving instance Storable e => Storable (Color OKLCH e)

-- | `OKLCH` color space
instance (Elevator e) => Show (Color OKLCH e) where
  showsPrec _ = showsColorModel

-- | Constructor for a OKL*a*b* color space in a cylindrical L*C*h parameterization
pattern ColorOKLCH :: e -> e -> e -> Color OKLCH e
pattern ColorOKLCH l c h = OKLCH (CM.ColorLCH l c h)
{-# COMPLETE ColorOKLCH #-}

-- | Constructor for a @OKLCH@ with alpha
pattern ColorOKLCHA :: e -> e -> e -> e -> Color (Alpha OKLCH) e
pattern ColorOKLCHA l c h a = Alpha (OKLCH (CM.ColorLCH l c h)) a
{-# COMPLETE ColorOKLCHA #-}

-- | `OKLCH` color space
instance (Elevator e, ColorModel OKLAB e) => ColorModel OKLCH e where
  type Components OKLCH e = (e, e, e)
  type ChannelCount OKLCH = 3
  channelCount _ = 3
  {-# INLINE channelCount #-}
  channelNames _ = "L" :| ["Cab", "Hab"]
  channelColors _ = V3 0x80 0x80 0x80 :|
                  [ V3 0xff 0x31 0x8e
                  , V3 0x66 0x66 0xff
                  ]
  toComponents (OKLCH lch) = toComponents lch
  {-# INLINE toComponents #-}
  fromComponents = OKLCH . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ =
    ("LCH-"++) . showsColorModelName (Proxy :: Proxy (Color OKLAB e))

instance (Elevator e, ColorSpace OKLAB D65 e) => ColorSpace OKLCH D65 e where
  type BaseModel OKLCH = CM.LCH
  type BaseSpace OKLCH = OKLAB
  toBaseSpace (OKLCH lch) = fmap fromDouble . fromComponents . CM.lch2lxy . fmap toDouble $ lch
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = OKLCH . fmap fromDouble . CM.lxy2lch . toComponents . fmap toDouble
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}
  grayscale (coerce -> V3 l _ _) = X l
  {-# INLINE grayscale #-}
  replaceGrayscale (coerce -> V3 _ c h) (X l) = coerce (V3 l c h)
  {-# INLINE replaceGrayscale #-}
