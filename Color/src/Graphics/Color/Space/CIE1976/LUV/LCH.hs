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
-- Module: Graphics.Color.Space.CIE1976.LUV.LCH

module Graphics.Color.Space.CIE1976.LUV.LCH
  ( pattern ColorLCH
  , pattern ColorLCHA
  , LCH
  , Color(LCHuv)
  ) where

import Data.Coerce
import Data.Proxy
import Foreign.Storable
import qualified Graphics.Color.Model.LCH as CM
import Graphics.Color.Space.CIE1976.LUV
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal

-- | [CIE L*C*Huv](https://en.wikipedia.org/wiki/CIELUV_color_space),
--   an LCH representation for the L*u*v* color space
data LCH (i :: k)

-- | Color in CIE L*C*Huv color space
newtype instance Color (LCH i) e = LCHuv (Color CM.LCH e)

-- | CIE1976 `LCH` color space
deriving instance Eq e => Eq (Color (LCH i) e)

-- | CIE1976 `LCH` color space
deriving instance Ord e => Ord (Color (LCH i) e)

-- | CIE1976 `LCH` color space
deriving instance Functor (Color (LCH i))

-- | CIE1976 `LCH` color space
deriving instance Applicative (Color (LCH i))

-- | CIE1976 `LCH` color space
deriving instance Foldable (Color (LCH i))

-- | CIE1976 `LCH` color space
deriving instance Traversable (Color (LCH i))

-- | CIE1976 `LCH` color space
deriving instance Storable e => Storable (Color (LCH i) e)

-- | CIE1976 `LCH` color space
instance (Illuminant i, Elevator e) => Show (Color (LCH i) e) where
  showsPrec _ = showsColorModel

-- | Constructor for a CIEL*u*v* color space in a cylindrical L*C*h parameterization
pattern ColorLCH :: e -> e -> e -> Color (LCH i) e
pattern ColorLCH l c h = LCHuv (CM.ColorLCH l c h)
{-# COMPLETE ColorLCH #-}

-- | Constructor for a @LCH@ with alpha
pattern ColorLCHA :: e -> e -> e -> e -> Color (Alpha (LCH i)) e
pattern ColorLCHA l c h a = Alpha (LCHuv (CM.ColorLCH l c h)) a
{-# COMPLETE ColorLCHA #-}

-- | CIE1976 `LCH` color space
instance (Illuminant i, Elevator e, ColorModel (LUV i) e) => ColorModel (LCH i) e where
  type Components (LCH i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ =
    ("LCH-"++) . showsColorModelName (Proxy :: Proxy (Color (LUV i) e))

instance (Illuminant i, Elevator e, ColorSpace (LUV i) i e) => ColorSpace (LCH i) i e where
  type BaseModel (LCH i) = CM.LCH
  type BaseSpace (LCH i) = LUV i
  toBaseSpace = fmap fromDouble . fromComponents . CM.lch2lxy . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.lxy2lch . toComponents . fmap toDouble
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}
