{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.ColorModel.Common
  ( module Graphics.ColorModel
  , colorModelSpec
  , toFromComponentsSpec
  , izipWithM_
  , epsilonExpect
  , epsilonPixelExpect
  , epsilonPixelIxSpec
  , epsilonEq
  , epsilonEqPixel
  , epsilonEqPixelTol
  , epsilonEqPixelTolIx
  , arbitraryElevator
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module System.Random
  ) where

import Control.Applicative
import Data.Foldable as F
import Graphics.ColorModel
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit (assertBool)
import Test.QuickCheck
import Control.Monad

izipWithM_ :: Applicative m => (Int -> a -> b -> m c) -> [a] -> [b] -> m ()
izipWithM_ f xs = zipWithM_ (uncurry f) (zip [0..] xs)

arbitraryElevator :: (Elevator e, Random e) => Gen e
arbitraryElevator = choose (minValue, maxValue)

epsilonExpect ::
     (HasCallStack, Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Expectation
epsilonExpect epsilon x y
  | isNaN x = y `shouldSatisfy` isNaN
  | x == y = pure ()
  | otherwise =
    assertBool (concat [show x, " /= ", show y, "\nTolerance: ", show diff, " > ", show n]) (diff <= n)
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)

epsilonPixelExpect ::
     (HasCallStack, ColorModel cs e, RealFloat e) => e -> Pixel cs e -> Pixel cs e -> Expectation
epsilonPixelExpect epsilon x y = zipWithM_ (epsilonExpect epsilon) (F.toList x) (F.toList y)

epsilonPixelIxSpec ::
     (HasCallStack, ColorModel cs e, RealFloat e)
  => e
  -> Int
  -> Pixel cs e
  -> Pixel cs e
  -> Spec
epsilonPixelIxSpec epsilon ix x y =
  it ("Index: " ++ show ix) $ zipWithM_ (epsilonExpect epsilon) (F.toList x) (F.toList y)


epsilonEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Property
epsilonEq epsilon x y = once $ epsilonExpect epsilon x y

epsilonEqPixel :: (ColorModel cs e, RealFloat e) => Pixel cs e -> Pixel cs e -> Property
epsilonEqPixel = epsilonEqPixelTol epsilon
  where
    epsilon = 1e-11

epsilonEqPixelTol :: (ColorModel cs e, RealFloat e) => e -> Pixel cs e -> Pixel cs e -> Property
epsilonEqPixelTol epsilon x y = conjoin $ F.toList $ liftA2 (epsilonEq epsilon) x y

-- | Same as `epsilonEqPixelTol` but with indexed counterexample.
epsilonEqPixelTolIx ::
     (ColorModel cs e, RealFloat e) => e -> Int -> Pixel cs e -> Pixel cs e -> Property
epsilonEqPixelTolIx tol ix expected actual =
  counterexample ("Index: " ++ show ix) $ epsilonEqPixelTol tol expected actual

prop_ToFromComponents ::
     forall cs e. ColorModel cs e
  => Pixel cs e
  -> Property
prop_ToFromComponents px = px === fromComponents (toComponents px)

toFromComponentsSpec :: forall cs e . (ColorModel cs e, Arbitrary (Pixel cs e)) => Spec
toFromComponentsSpec = prop "fromComponents . toComponents" $ prop_ToFromComponents @cs @e


colorModelSpec :: forall cs e . (ColorModel cs e, Arbitrary (Pixel cs e)) => Spec
colorModelSpec =
  describe "ColorModel" $
    toFromComponentsSpec @cs @e
