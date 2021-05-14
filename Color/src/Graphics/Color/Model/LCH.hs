{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module : Graphics.Color.Model.LCH
module Graphics.Color.Model.LCH
  ( LCH
  -- * Constructors for an LCH color model.
  , pattern ColorLCH
  , pattern ColorLCHA
  , Color
  , ColorModel(..)
  , lch2lxy
  , lxy2lch
  ) where

import Data.Complex ( Complex(..), polar, mkPolar )
import Data.Fixed ( mod' )
import Foreign.Storable
import Graphics.Color.Model.Internal

-----------
--- LCH ---
-----------

-- | CIEL*C*H color model, representing a cylindrical reparameterization
--   of CIEL*a*b* or CIEL*u*v*.
data LCH

-- | `LCH` color model
newtype instance Color LCH e = LCH (V3 e)

-- | Constructor for @LCH@.
pattern ColorLCH :: e -> e -> e -> Color LCH e
pattern ColorLCH l c h = LCH (V3 l c h)
{-# COMPLETE ColorLCH #-}


-- | Constructor for @LCH@ with alpha channel.
pattern ColorLCHA :: e -> e -> e -> e -> Color (Alpha LCH) e
pattern ColorLCHA l c h a = Alpha (ColorLCH l c h) a
{-# COMPLETE ColorLCHA #-}

-- | `LCH` color model
deriving instance Eq e => Eq (Color LCH e)
-- | `LCH` color model
deriving instance Ord e => Ord (Color LCH e)
-- | `LCH` color model
deriving instance Functor (Color LCH)
-- | `LCH` color model
deriving instance Applicative (Color LCH)
-- | `LCH` color model
deriving instance Foldable (Color LCH)
-- | `LCH` color model
deriving instance Traversable (Color LCH)
-- | `LCH` color model
deriving instance Storable e => Storable (Color LCH e)

-- | `LCH` color model
instance Elevator e => Show (Color LCH e) where
  showsPrec _ = showsColorModel

-- | `LCH` color model
instance Elevator e => ColorModel LCH e where
  type Components LCH e = (e, e, e)
  toComponents (ColorLCH l c h) = (l, c, h)
  {-# INLINE toComponents #-}
  fromComponents (l, c, h) = ColorLCH l c h
  {-# INLINE fromComponents #-}

lch2lxy :: Color LCH Double -> Components LCH Double
lch2lxy (ColorLCH l c h) = (l, x, y)
 where
  !h' = h * pi / 180
  (x :+ y) = mkPolar c h'
{-# INLINE lch2lxy #-}

lxy2lch :: Components LCH Double -> Color LCH Double
lxy2lch (l, x, y) = ColorLCH l c h
 where
  (c,h') = polar (x :+ y)
  !h = (h' * 180 / pi) `mod'` 360
{-# INLINE lxy2lch #-}
