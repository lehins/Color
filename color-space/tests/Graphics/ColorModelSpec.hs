{-# LANGUAGE FlexibleInstances #-}
module Graphics.ColorModelSpec
  ( spec
  , module Graphics.ColorModel
  , epsilonEq
  , epsilonEqPixel
  , epsilonEqPixelTol
  , epsilonEqPixelTolIx
  , arbitraryElevator
  ) where

import Control.Applicative
import Data.Foldable as F
import Graphics.ColorModel
import System.Random
import Test.Hspec
import Test.QuickCheck


arbitraryElevator :: (Elevator e, Random e) => Gen e
arbitraryElevator = choose (minValue, maxValue)

epsilonEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Property
epsilonEq epsilon x y =
  x === y .||. counterexample (show diff ++ " > " ++ show n) (diff <= n) .||. (isNaN x && isNaN y)
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)

epsilonEqPixel :: (ColorModel cs e, RealFloat e) => Pixel cs e -> Pixel cs e -> Property
epsilonEqPixel = epsilonEqPixelTol epsilon
  where
    epsilon = 1e-13

epsilonEqPixelTol :: (ColorModel cs e, RealFloat e) => e -> Pixel cs e -> Pixel cs e -> Property
epsilonEqPixelTol epsilon x y = conjoin $ F.toList $ liftA2 (epsilonEq epsilon) x y

-- | Same as `epsilonEqPixelTol` but with indexed counterexample.
epsilonEqPixelTolIx ::
     (ColorModel cs e, RealFloat e) => e -> Int -> Pixel cs e -> Pixel cs e -> Property
epsilonEqPixelTolIx tol ix expected actual =
  counterexample ("Index: " ++ show ix) $ epsilonEqPixelTol tol expected actual


spec :: Spec
spec = pure ()
