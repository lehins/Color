{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Graphics.Color.Space.CIE1976.LAB.LCH

module Graphics.Color.Space.CIE1976.LAB.LCH
  ( pattern ColorLCH
  , pattern ColorLCHA
  , LCH
  , Color(LCHab)
  ) where

import Data.Coerce
import Data.Proxy
import Foreign.Storable
import qualified Graphics.Color.Model.LCH as CM
import Graphics.Color.Space.CIE1976.LAB
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal

-- | [CIE L*C*Hab](https://en.wikipedia.org/wiki/CIELAB_color_space),
--   an LCH representation for the L*a*b* color space
data LCH (i :: k)

-- | Color in CIE L*C*Hab color space
newtype instance Color (LCH i) e = LCHab (Color CM.LCH e)

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

-- | Constructor for a CIEL*a*b* color space in a cylindrical L*C*h parameterization
pattern ColorLCH :: e -> e -> e -> Color (LCH i) e
pattern ColorLCH l c h = LCHab (CM.ColorLCH l c h)
{-# COMPLETE ColorLCH #-}

-- | Constructor for a @LCH@ with alpha
pattern ColorLCHA :: e -> e -> e -> e -> Color (Alpha (LCH i)) e
pattern ColorLCHA l c h a = Alpha (LCHab (CM.ColorLCH l c h)) a
{-# COMPLETE ColorLCHA #-}

-- | CIE1976 `LCH` color space
instance (Illuminant i, Elevator e, ColorModel (LAB i) e) => ColorModel (LCH i) e where
  type Components (LCH i) e = (e, e, e)
  toComponents = toComponents . coerce
  {-# INLINE toComponents #-}
  fromComponents = coerce . fromComponents
  {-# INLINE fromComponents #-}
  showsColorModelName _ =
    ("LCH-"++) . showsColorModelName (Proxy :: Proxy (Color (LAB i) e))

instance (Illuminant i, Elevator e, ColorSpace (LAB i) i e) => ColorSpace (LCH i) i e where
  type BaseModel (LCH i) = CM.LCH
  type BaseSpace (LCH i) = LAB i
  toBaseSpace = fmap fromDouble . fromComponents . CM.lch2lxy . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.lxy2lch . toComponents . fmap toDouble
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . toBaseSpace
  {-# INLINE luminance #-}
