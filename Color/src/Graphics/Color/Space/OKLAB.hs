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
-- |
-- Module      : Graphics.Color.Space.OKLAB
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.OKLAB
  ( -- * Constructors for an CIE L*a*b* color space.
    pattern OKLAB
  , pattern ColorOKLAB
  , pattern ColorOKLABA
  , OKLAB
  ) where

import Data.List.NonEmpty
import Foreign.Storable
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal
import Graphics.Color.Illuminant.ITU.Rec601 (D65)

--------------
--- OKLAB ---
--------------

-- | [OK L*a*b*](https://bottosson.github.io/posts/oklab/) color space
--
-- Values for matrices used in conversion to and from @CIEXYZ@ where taken from
-- [W3 CSS specification](https://www.w3.org/TR/css-color-4/#color-conversion-code) 
-- instead of the original blogpost about the OKLab color space.
data OKLAB

-- | Color in OK L*a*b* color space
newtype instance Color OKLAB e = OKLAB (V3 e)


pattern ColorOKLAB :: e -> e -> e -> Color OKLAB e
pattern ColorOKLAB l' a' b' = OKLAB (V3 l' a' b')
{-# COMPLETE ColorOKLAB #-}

-- | Constructor for @OKLAB@ with alpha channel.
pattern ColorOKLABA :: e -> e -> e -> e -> Color (Alpha OKLAB) e
pattern ColorOKLABA l' a' b' a = Alpha (OKLAB (V3 l' a' b')) a
{-# COMPLETE ColorOKLABA #-}

-- | `OKLAB` color space
deriving instance Eq e => Eq (Color OKLAB e)

-- | `OKLAB` color space
deriving instance Ord e => Ord (Color OKLAB e)

-- | `OKLAB` color space
deriving instance Functor (Color OKLAB)

-- | `OKLAB` color space
deriving instance Applicative (Color OKLAB)

-- | `OKLAB` color space
deriving instance Foldable (Color OKLAB)

-- | `OKLAB` color space
deriving instance Traversable (Color OKLAB)

-- | `OKLAB` color space
deriving instance Storable e => Storable (Color OKLAB e)

-- | `OKLAB` color space
instance (Elevator e) => Show (Color OKLAB e) where
  showsPrec _ = showsColorModel

-- | `OKLAB` color space
instance (Elevator e) => ColorModel OKLAB e where
  type Components OKLAB e = (e, e, e)
  type ChannelCount OKLAB = 3
  channelCount _ = 3
  {-# INLINE channelCount #-}
  channelNames _ = "L*" :| ["a*", "b*"]
  channelColors _ = V3 0x80 0x80 0x80 :| -- gray
                  [ V3 0xb5 0x00 0x5e    -- dark magenta
                  , V3 0x00 0x00 0x8b    -- dark gold
                  ]
  toComponents (ColorOKLAB l' a' b') = (l', a', b')
  {-# INLINE toComponents #-}
  fromComponents (l', a', b') = ColorOKLAB l' a' b'
  {-# INLINE fromComponents #-}

instance (Elevator e, RealFloat e) => ColorSpace OKLAB D65 e where
  type BaseModel OKLAB = OKLAB
  type BaseSpace OKLAB = OKLAB
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance c = Y $ lab2luminance c
  {-# INLINE luminance #-}
  toColorXYZ = lab2xyz
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = xyz2lab
  {-# INLINE fromColorXYZ #-}
  grayscale (ColorOKLAB l' _ _) = X l'
  {-# INLINE grayscale #-}
  replaceGrayscale (ColorOKLAB _ a' b') (X l') = ColorOKLAB l' a' b'
  {-# INLINE replaceGrayscale #-}

lab2luminance :: forall a e. (Elevator e, Elevator a, RealFloat e, RealFloat a)
  => Color OKLAB e
  -> a
lab2luminance (ColorOKLAB ll a b) = toRealFloat y
  where
    !l' = ll + 0.3963377773761749 * a + 0.2158037573099136 * b
    !m' = ll - 0.1055613458156586 * a - 0.0638541728258133 * b
    !s' = ll - 0.0894841775298119 * a - 1.2914855480194092 * b
    !l = l' ^ (3 :: Int)
    !m = m' ^ (3 :: Int)
    !s = s' ^ (3 :: Int)
    !y = (-0.0405757452148008) * l + 1.1122868032803170 * m - 0.0717110580655164 * s
{-# INLINE lab2luminance #-}

lab2xyz ::
     forall a e. (Elevator e, Elevator a, RealFloat e, RealFloat a)
  => Color OKLAB e
  -> Color (XYZ D65) a
lab2xyz (ColorOKLAB ll a b) = ColorXYZ rfX rfY rfZ
  where
    !l' = ll + 0.3963377773761749 * a + 0.2158037573099136 * b
    !m' = ll - 0.1055613458156586 * a - 0.0638541728258133 * b
    !s' = ll - 0.0894841775298119 * a - 1.2914855480194092 * b
    !l = l' ^ (3 :: Int)
    !m = m' ^ (3 :: Int)
    !s = s' ^ (3 :: Int)
    !x =  1.2268798758459243   * l - 0.5578149944602171 * m + 0.2813910456659647 * s
    !y = (-0.0405757452148008) * l + 1.1122868032803170 * m - 0.0717110580655164 * s
    !z = (-0.0763729366746601) * l - 0.4214933324022432 * m + 1.5869240198367816 * s
    !rfX = toRealFloat x
    !rfY = toRealFloat y
    !rfZ = toRealFloat z
{-# INLINE lab2xyz #-}

signedCubeRoot :: RealFloat e => e -> e
signedCubeRoot x =
  if x < 0
    then -((-x) ** (1 / 3))
    else x ** (1 / 3)
{-# INLINE signedCubeRoot #-}

xyz2lab ::
     forall a e. (Elevator a, Elevator e, RealFloat a, RealFloat e)
  => Color (XYZ D65) a
  -> Color OKLAB e
xyz2lab (ColorXYZ x y z) = ColorOKLAB rfL rfA rfB
  where
    !l = 0.8190224379967030 * x + 0.3619062600528904 * y - 0.1288737815209879 * z
    !m = 0.0329836539323885 * x + 0.9292868615863434 * y + 0.0361446663506424 * z
    !s = 0.0481771893596242 * x + 0.2642395317527308 * y + 0.6335478284694309 * z
    !l' = signedCubeRoot l
    !m' = signedCubeRoot m
    !s' = signedCubeRoot s
    !ll = 0.2104542683093140 * l' + 0.7936177747023054 * m' - 0.0040720430116193 * s'
    !a  = 1.9779985324311684 * l' - 2.4285922420485799 * m' + 0.4505937096174110 * s'
    !b  = 0.0259040424655478 * l' + 0.7827717124575296 * m' - 0.8086757549230774 * s'
    !rfL = toRealFloat ll
    !rfA = toRealFloat a
    !rfB = toRealFloat b
{-# INLINE xyz2lab #-}
