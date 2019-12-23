{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Adaptation.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Adaptation.Internal
  ( ChromaticAdaptation(..)
  , chromaticAdaptation
  , convertWith
  , convertElevatedWith
  , convertNoAdaptation
  ) where

import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal
import Data.Kind


class (Illuminant it, Illuminant ir, Elevator e, RealFloat e) =>
      ChromaticAdaptation (t :: k) (it :: kt) (ir :: kr) e
  where
  data Adaptation t it ir e :: Type
  adaptColorXYZ :: Adaptation t it ir e -> Color (XYZ it) e -> Color (XYZ ir) e


chromaticAdaptation ::
     ChromaticAdaptation t it ir e
  => Adaptation t it ir e
  -> Gamut cst it e
  -> Gamut csr ir e
chromaticAdaptation adaptation g = Gamut redPrimary greenPrimary bluePrimary
  where
    applyMatrix primary =
      PrimaryChromaticity
        (Chromaticity (fromColorXYZ (convertWith adaptation (primaryTristimulus primary))))
    redPrimary = applyMatrix (gamutRedPrimary g)
    greenPrimary = applyMatrix (gamutGreenPrimary g)
    bluePrimary = applyMatrix (gamutBluePrimary g)


convertWith ::
     (ChromaticAdaptation t i' i e, ColorSpace cs' i' e, ColorSpace cs i e)
  => Adaptation t i' i e
  -> Color cs' e
  -> Color cs e
convertWith = convertElevatedWith
{-# INLINE convertWith #-}

convertElevatedWith ::
     (ChromaticAdaptation t i' i a, ColorSpace cs' i' e', ColorSpace cs i e)
  => Adaptation t i' i a
  -> Color cs' e'
  -> Color cs e
convertElevatedWith adaptation = fromColorXYZ . adaptColorXYZ adaptation . toColorXYZ
{-# INLINE[2] convertElevatedWith #-}

-- | Convert a color from one color space into another one with the same illuminant, thus
-- not requireing any chromatic adaptation.
--
-- @since 0.1.0
convertNoAdaptation ::
     forall cs' e' cs e i. (ColorSpace cs' i e', ColorSpace cs i e)
  => Color cs' e'
  -> Color cs e
convertNoAdaptation = fromColorXYZ . (toColorXYZ :: Color cs' e' -> Color (XYZ i) Double)
{-# INLINE convertNoAdaptation #-}

convertNoAdaptationFloat ::
     forall cs' e' cs e i. (ColorSpace cs' i e', ColorSpace cs i e)
  => Color cs' e'
  -> Color cs e
convertNoAdaptationFloat = fromColorXYZ . (toColorXYZ :: Color cs' e' -> Color (XYZ i) Float)
{-# INLINE convertNoAdaptationFloat #-}

{-# RULES
"convertElevatedWith (Float)"[~2] forall (a :: Adaptation t i i Float) . convertElevatedWith a = convertNoAdaptationFloat
"convertElevatedWith (Double)"[~2] forall (a :: Adaptation t i i Double) . convertElevatedWith a = convertNoAdaptation
#-}


-- toword8 <$> (fromColorXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toColorXYZ (ColorLAB 76.022 (-0.366) 27.636 :: Color (LAB D50a) Double) :: Color XYZ Double)) :: Color SRGB Double)
