{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeFamilies #-}

module Graphics.Color.Space.DIN99
  ( pattern DIN99,
    pattern ColorDIN99,
    DIN99,
    deltaE,
  )
where

import Foreign.Storable
import GHC.Generics (Generic)
import Graphics.Color.Illuminant.Wikipedia as W
import Graphics.Color.Model.Internal
import Graphics.Color.Space.Internal
import Graphics.Color.Space.CIE1976.LAB

data DIN99 (i :: k)

-- | Color in DIN99 color space
newtype instance Color (DIN99 i) e = DIN99 (V3 e)

pattern ColorDIN99 :: e -> e -> e -> Color (DIN99 i) e
pattern ColorDIN99 l' a' b' = DIN99 (V3 l' a' b')

{-# COMPLETE ColorDIN99 #-}

-- | `DIN99` color space
deriving instance Eq e => Eq (Color (DIN99 i) e)

-- | `DIN99` color space
deriving instance Ord e => Ord (Color (DIN99 i) e)

-- | `DIN99` color space
deriving instance Functor (Color (DIN99 i))

-- | `DIN99` color space
deriving instance Applicative (Color (DIN99 i))

-- | `DIN99` color space
deriving instance Foldable (Color (DIN99 i))

-- | `DIN99` color space
deriving instance Traversable (Color (DIN99 i))

-- | `DIN99` color space
deriving instance Storable e => Storable (Color (DIN99 i) e)

-- | `DIN99` color space
instance (Illuminant i, Elevator e) => Show (Color (DIN99 i) e) where
  showsPrec _ = showsColorModel

instance (Illuminant i, Elevator e) => ColorModel (DIN99 i) e where
  type Components (DIN99 i) e = (e, e, e)
  toComponents (ColorDIN99 l' a' b') = (l', a', b')
  {-# INLINE toComponents #-}
  fromComponents (l', a', b') = ColorDIN99 l' a' b'
  {-# INLINE fromComponents #-}

instance (Illuminant i, Elevator e, RealFloat e) => ColorSpace (DIN99 (i :: k)) i e where
  type BaseModel (DIN99 i) = DIN99 i
  type BaseSpace (DIN99 i) = DIN99 i
  toBaseSpace = id
  {-# INLINE toBaseSpace #-}
  fromBaseSpace = id
  {-# INLINE fromBaseSpace #-}
  luminance = luminance . dinToLAB DIN99Method
  {-# INLINE luminance #-}
  toColorXYZ = toColorXYZ . dinToLAB DIN99Method
  {-# INLINE toColorXYZ #-}
  fromColorXYZ = labToDIN DIN99Method . fromColorXYZ
  {-# INLINE fromColorXYZ #-}

kE :: RealFloat a => a
kE = 1

kCH :: RealFloat a => a
kCH = 1

data DINMethod
  = ASTMD2244_07Method
  | DIN99Method
  | DIN99bMethod
  | DIN99cMethod
  | DIN99dMethod
  deriving (Eq, Show, Read, Ord, Bounded, Enum, Generic)

fromDINMethod ::
  ( Fractional a,
    Fractional b,
    Fractional c,
    Fractional d,
    Fractional f,
    Fractional g,
    Fractional h,
    Fractional e
  ) =>
  DINMethod ->
  (a, b, c, d, e, f, g, h)
fromDINMethod ASTMD2244_07Method =
  (105.509, 0.0158, 16.0, 0.7, 1, 9 / 200, 0.0, 9 / 200)
fromDINMethod DIN99Method = (105.509, 0.0158, 16.0, 0.7, 1, 9 / 200, 0.0, 9 / 200)
fromDINMethod DIN99bMethod = (303.67, 0.0039, 26.0, 0.83, 23.0, 0.075, 26.0, 1)
fromDINMethod DIN99cMethod = (317.65, 0.0037, 0.0, 0.94, 23.0, 0.066, 0.0, 1)
fromDINMethod DIN99dMethod = (325.22, 0.0036, 50.0, 1.14, 22.5, 0.06, 50.0, 1)

radians :: Floating a => a -> a
radians x = pi / 180 * x

-- | deltaE distance in DIN99 is the perceptually-uniform distance between
-- two points in the space. Otherwise known as euclidean distance.
deltaE ::
  (Floating e, Illuminant i, Elevator e) =>
  Color (LAB i) e ->
  Color (LAB i) e ->
  e
deltaE a b = sqrt $ sum ((a - b) ** 2)

-- | Bidirectional @DIN99@ @DIN99@ conversion as implemented in
-- https://github.com/colour-science/colour/blob/c3735e5d0ad67443022ece0b42b575e040eb61d1/colour/models/din99.py#L79
-- >>> labToDIN DIN99Method (ColorLAB 41.52787529 52.63858304 26.92317922 :: Color (LAB 'W.D65) Double)
-- <DIN99 Degree2 'D65:(53.2282198832885240,28.4163465573069870, 3.8983955176918417)>
-- >>> dinToLAB DIN99Method (ColorDIN99 53.22821988 28.41634656 3.89839552 :: Color (DIN99 'W.D65) Double)
-- <LAB Degree2 'D65:(41.5278752867329700,52.6385830477006900,26.9231792301717970)>
labToDIN ::
  (RealFloat e) =>
  DINMethod ->
  Color (LAB i) e ->
  Color (DIN99 i) e
labToDIN m (ColorLAB l a b) = ColorDIN99 l_99 a_99 b_99
  where
    cos_c = cos $ radians c_3
    sin_c = sin $ radians c_3
    e = cos_c * a + sin_c * b
    f = c_4 * (- sin_c * a + cos_c * b)
    g' = sqrt (e ** 2 + f ** 2)
    h_ef = atan2 f e + radians c_7
    c_99 = c_5 * log (1 + c_6 * g') / (c_8 * kCH * kE)
    a_99 = c_99 * cos h_ef
    b_99 = c_99 * sin h_ef
    l_99 = c_1 * log (1 + c_2 * l) * kE
    (c_1, c_2, c_3, c_4, c_5, c_6, c_7, c_8) = fromDINMethod m

dinToLAB ::
  RealFloat e =>
  DINMethod ->
  Color (DIN99 i) e ->
  Color (LAB i) e
dinToLAB m (ColorDIN99 l_99 a_99 b_99) = ColorLAB l a b
  where
    expm1 x = exp x - 1
    cos' = cos (radians c_3)
    sin' = sin (radians c_3)

    h_99 = atan2 b_99 a_99 - radians c_7

    c_99 = sqrt (a_99 ** 2 + b_99 ** 2)
    g' = expm1 (c_8 / c_5 * c_99 * kCH * kE) / c_6

    e = g' * cos h_99
    f = g' * sin h_99

    a = e * cos' - f / c_4 * sin'
    b = e * sin' + f / c_4 * cos'
    l = expm1 (l_99 * kE / c_1) / c_2
    (c_1, c_2, c_3, c_4, c_5, c_6, c_7, c_8) = fromDINMethod m
