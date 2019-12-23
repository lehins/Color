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
  ( ColorAdaptation(..)
  , chromaticityAdaptation
  , convertColorSpace
  ) where

import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal
import Data.Kind


class (Illuminant it, Illuminant ir, Elevator e, RealFloat e) =>
      ColorAdaptation (t :: k) (it :: kt) (ir :: kr) e
  where
  data Adaptation t it ir e :: Type
  adaptPixelXYZ :: Adaptation t it ir e -> Pixel (XYZ it) e -> Pixel (XYZ ir) e


chromaticityAdaptation ::
     ColorAdaptation t it ir e
  => Adaptation t it ir e
  -> Chromaticity cst it e
  -> Chromaticity csr ir e
chromaticityAdaptation param c = Chromaticity redPrimary greenPrimary bluePrimary
  where
    applyMatrix chroma = PrimaryChroma (fromPixelXYZ (adaptPixelXYZ param (primaryXYZ chroma)))
    redPrimary = applyMatrix (chromaRed c)
    greenPrimary = applyMatrix (chromaGreen c)
    bluePrimary = applyMatrix (chromaBlue c)

convertColorSpace ::
     (ColorAdaptation t i2 i1 a, ColorSpace cs1 i1 e1, ColorSpace cs2 i2 e2)
  => Adaptation t i2 i1 a
  -> Pixel cs2 e2
  -> Pixel cs1 e1
convertColorSpace param = fromPixelXYZ . adaptPixelXYZ param . toPixelXYZ





-- toWord8 <$> (fromPixelXYZ (chromaticAdaptationXYZ (vonKriesAdaptationMatrix :: VonKriesAdaptationMatrix Bradford D50a D65 Double) (toPixelXYZ (PixelLAB 76.022 (-0.366) 27.636 :: Pixel (LAB D50a) Double) :: Pixel XYZ Double)) :: Pixel SRGB Double)
