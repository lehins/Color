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
import qualified Graphics.Color.Model.RGB as CM
import Graphics.Color.Model
import Graphics.Color.Space
import Graphics.Color.Space.RGB
import Graphics.Color.Illuminant.CIE1931
import Graphics.Color.Space.CIE1976.LAB
import qualified Graphics.Color.Space.RGB.Derived.SRGB as Derived

import qualified Data.Colour as Colour
import qualified Data.Colour.SRGB as Colour
import qualified Data.Colour.CIE as Colour

main :: IO ()
main =
  defaultMain $
  map
    fst
    [ mkBenchmarks (0 :: Float) "Float" gen
    , mkBenchmarks (0 :: Double) "Double" gen
    , mkBenchmarksLAB (0 :: Double) "L*a*b*" gen
    ]
  where
    gen = mkStdGen 2019

randomV3 :: (RandomGen g, Random a, Elevator a) => g -> (V3 a, g)
randomV3 g0 = (V3 v0 v1 v2, g3)
  where
    (v0, g1) = randomR (minValue, maxValue) g0
    (v1, g2) = randomR (minValue, maxValue) g1
    (v2, g3) = randomR (minValue, maxValue) g2

makeRandomColor :: forall cs i a g . (ColorSpace cs i a, RandomGen g) => g -> (Color cs a, g)
makeRandomColor gen =
  case randomV3 gen of
    (V3 x y z, gen') -> (fromColorXYZ (ColorXYZ x y z :: Color (XYZ i) Double), gen')


makeRandomRGB ::
     (RedGreenBlue cs (i :: k), RandomGen g, Random a, Elevator a) => g -> (Color cs a, g)
makeRandomRGB gen =
  case randomV3 gen of
    (V3 r g b, gen') -> (mkColorRGB (CM.ColorRGB r g b), gen')

makeRandomColour ::
     (RandomGen b1, Random b2, Elevator b2, Ord b2, Floating b2) => b1 -> (Colour.Colour b2, b1)
makeRandomColour gen =
  case randomV3 gen of
    (V3 r g b, gen') -> (Colour.sRGB r g b, gen')


sameAs :: a -> (b -> a) -> b -> a
sameAs _ = ($)

mkBenchmarks ::
     forall f g. (RandomGen g, Random f, Elevator f, RealFloat f, NFData f)
  => f
  -> String
  -> g
  -> (Benchmark, g)
mkBenchmarks _ tyName g0 =
  let !(srgb :: Color SRGB f, g1) = makeRandomRGB g0
      !(srgbDerived :: Color (Derived.SRGB 'D65) f, g2) = makeRandomRGB g1
      !(xyz :: Color (XYZ D65) f) = toColorXYZ srgb
      !(xyzDerived :: Color (XYZ 'D65) f) = toColorXYZ srgbDerived
      !(srgbColour :: Colour.Colour f, g3) = makeRandomColour g2
      xyzColour@(!_, !_, !_) = Colour.cieXYZView srgbColour
   in ( bgroup
          ("sRGB " <> tyName)
          [ bgroup
              "Standard"
              [ bench "toColorXYZ" $ nf (sameAs xyz toColorXYZ) srgb
              , bench "fromColorXYZ" $ nf (sameAs srgb fromColorXYZ) xyz
              ]
          , bgroup
              "Derived"
              [ bench "toColorXYZ" $ nf (sameAs xyzDerived toColorXYZ) srgbDerived
              , bench "fromColorXYZ" $ nf (sameAs srgbDerived fromColorXYZ) xyzDerived
              ]
          , bgroup
              "Colour"
              [ bench "cieXYZView" $ nf Colour.cieXYZView srgbColour
              , bench "cieXYZ" $ whnf (\(x, y, z) -> Colour.cieXYZ x y z) xyzColour
              ]
          ]
      , g3)


mkBenchmarksLAB ::
     forall f g. (RandomGen g, Elevator f, RealFloat f, NFData f)
  => f
  -> String
  -> g
  -> (Benchmark, g)
mkBenchmarksLAB _ tyName g0 =
  let !(lab :: Color (LAB 'D65) f, g1) = makeRandomColor g0
      !(xyz :: Color (XYZ 'D65) f) = toColorXYZ lab
   in ( bgroup
          ("LAB " <> tyName)
          [ bgroup
              "Standard"
              [ bench "toColorXYZ" $ nf (sameAs xyz toColorXYZ) lab
              , bench "fromColorXYZ" $ nf (sameAs lab fromColorXYZ) xyz
              ]
          ]
      , g1)
