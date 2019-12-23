{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
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
  , convertColor
  , convertColorStrict
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
chromaticAdaptation param g = Gamut redPrimary greenPrimary bluePrimary
  where
    applyMatrix primary =
      PrimaryChromaticity
        (Chromaticity (fromColorXYZ (convertColorStrict param (primaryTristimulus primary))))
    redPrimary = applyMatrix (gamutRedPrimary g)
    greenPrimary = applyMatrix (gamutGreenPrimary g)
    bluePrimary = applyMatrix (gamutBluePrimary g)

convertColor ::
     (ChromaticAdaptation t i2 i1 a, ColorSpace cs1 i1 e1, ColorSpace cs2 i2 e2)
  => Adaptation t i2 i1 a
  -> Color cs2 e2
  -> Color cs1 e1
convertColor param = fromColorXYZ . adaptColorXYZ param . toColorXYZ


convertColorStrict ::
     (ChromaticAdaptation t i2 i1 e, ColorSpace cs1 i1 e, ColorSpace cs2 i2 e)
  => Adaptation t i2 i1 e
  -> Color cs2 e
  -> Color cs1 e
convertColorStrict param = fromColorXYZ . adaptColorXYZ param . toColorXYZ





-- toWord8 <$> (fromColorXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toColorXYZ (ColorLAB 76.022 (-0.366) 27.636 :: Color (LAB D50a) Double) :: Color XYZ Double)) :: Color SRGB Double)
