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
import Graphics.Color.Model.Alpha
import qualified Graphics.Color.Model.CMYK as CM
import Graphics.Color.Model.Internal
import Graphics.Color.Space

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
instance (Typeable cs, ColorSpace cs i e, RedGreenBlue cs i) => ColorSpace (CMYK cs) i e where
  type BaseModel (CMYK cs) = CM.CMYK
  type BaseSpace (CMYK cs) = cs
  toBaseSpace = mkColorRGB . fmap fromDouble . CM.cmyk2rgb . fmap toDouble . coerce
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = coerce . fmap fromDouble . CM.rgb2cmyk . fmap toDouble . unColorRGB
  {-# INLINE fromBaseSpace #-}
