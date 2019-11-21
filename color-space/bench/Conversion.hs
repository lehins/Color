{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Control.DeepSeq
import System.Random as R
import qualified Graphics.ColorModel.RGB as CM
import Graphics.ColorModel
import Graphics.ColorSpace
import Graphics.ColorSpace.Algebra
import Graphics.ColorSpace.RGB
import Graphics.ColorSpace.CIE1931.Illuminant
import qualified Graphics.ColorSpace.RGB.Derived.SRGB as Derived

import qualified Data.Colour as Colour
import qualified Data.Colour.SRGB as Colour
import qualified Data.Colour.CIE as Colour

main :: IO ()
main = defaultMain [mkBenchmarks (0 :: Float) "Float", mkBenchmarks (0 :: Double) "Double"]

randomV3 :: (RandomGen g, Random a, Elevator a) => g -> (V3 a, g)
randomV3 g0 = (V3 v0 v1 v2, g3)
  where
    (v0, g1) = randomR (minValue, maxValue) g0
    (v1, g2) = randomR (minValue, maxValue) g1
    (v2, g3) = randomR (minValue, maxValue) g2

makeRandomRGB ::
     (RedGreenBlue cs (i :: k), RandomGen g, Random a, Elevator a) => g -> (Pixel cs a, g)
makeRandomRGB gen =
  case randomV3 gen of
    (V3 r g b, gen') -> (mkPixelRGB (CM.PixelRGB r g b), gen')

makeRandomColour ::
     (RandomGen b1, Random b2, Elevator b2, Ord b2, Floating b2) => b1 -> (Colour.Colour b2, b1)
makeRandomColour gen =
  case randomV3 gen of
    (V3 r g b, gen') -> (Colour.sRGB r g b, gen')


sameAs :: a -> (b -> a) -> b -> a
sameAs _ = ($)

mkBenchmarks :: forall f. (Random f, Elevator f, RealFloat f, NFData f) => f -> String -> Benchmark
mkBenchmarks _ tyName =
  let g0 = mkStdGen 2019
      !(srgb :: Pixel SRGB f, g1) = makeRandomRGB g0
      !(srgbDerived :: Pixel (Derived.SRGB 'D65) f, g2) = makeRandomRGB g1
      !(xyz :: Pixel XYZ f) = toPixelXYZ srgb
      !(srgbColour :: Colour.Colour f, _g3) = makeRandomColour g2
      xyzColour@(!_, !_, !_) = Colour.cieXYZView srgbColour
   in bgroup
        ("sRGB " <> tyName)
        [ bgroup
            "Standard"
            [ bench "toPixelXYZ" $ nf (sameAs xyz toPixelXYZ) srgb
            , bench "fromPixelXYZ" $ nf (sameAs srgb fromPixelXYZ) xyz
            ]
        , bgroup
            "Derived"
            [ bench "toPixelXYZ" $ nf (sameAs xyz toPixelXYZ) srgbDerived
            , bench "fromPixelXYZ" $ nf (sameAs srgbDerived fromPixelXYZ) xyz
            ]
        , bgroup
            "Colour"
            [ bench "cieXYZView" $ nf Colour.cieXYZView srgbColour
            , bench "cieXYZ" $ whnf (\(x, y, z) -> Colour.cieXYZ x y z) xyzColour
            ]
        ]
