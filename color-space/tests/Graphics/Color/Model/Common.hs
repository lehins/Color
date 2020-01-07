{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.Common
  ( colorModelSpec
  , toFromComponentsSpec
  , izipWithM_
  , epsilonExpect
  , epsilonColorExpect
  , epsilonColorIxSpec
  , epsilonEq
  , epsilonEqColor
  , epsilonEqColorTol
  , epsilonEqColorTolIx
  , arbitraryElevator
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module System.Random
  ) where

import Control.Applicative
import Data.Proxy
import Data.Foldable as F
import Graphics.Color.Model
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

epsilonColorExpect ::
     (HasCallStack, ColorModel cs e, RealFloat e) => e -> Color cs e -> Color cs e -> Expectation
epsilonColorExpect epsilon x y = zipWithM_ (epsilonExpect epsilon) (F.toList x) (F.toList y)

epsilonColorIxSpec ::
     (HasCallStack, ColorModel cs e, RealFloat e)
  => e
  -> Int
  -> Color cs e
  -> Color cs e
  -> Spec
epsilonColorIxSpec epsilon ix x y =
  it ("Index: " ++ show ix) $ zipWithM_ (epsilonExpect epsilon) (F.toList x) (F.toList y)


epsilonEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Property
epsilonEq epsilon x y = once $ epsilonExpect epsilon x y

epsilonEqColor :: (ColorModel cs e, RealFloat e) => Color cs e -> Color cs e -> Property
epsilonEqColor = epsilonEqColorTol epsilon
  where
    epsilon = 1e-11

epsilonEqColorTol :: (ColorModel cs e, RealFloat e) => e -> Color cs e -> Color cs e -> Property
epsilonEqColorTol epsilon x y = conjoin $ F.toList $ liftA2 (epsilonEq epsilon) x y

-- | Same as `epsilonEqColorTol` but with indexed counterexample.
epsilonEqColorTolIx ::
     (ColorModel cs e, RealFloat e) => e -> Int -> Color cs e -> Color cs e -> Property
epsilonEqColorTolIx tol ix expected actual =
  counterexample ("Index: " ++ show ix) $ epsilonEqColorTol tol expected actual

prop_ToFromComponents ::
     forall cs e. ColorModel cs e
  => Color cs e
  -> Property
prop_ToFromComponents px = px === fromComponents (toComponents px)

toFromComponentsSpec :: forall cs e . (ColorModel cs e, Arbitrary (Color cs e)) => Spec
toFromComponentsSpec = prop "fromComponents . toComponents" $ prop_ToFromComponents @cs @e


colorModelSpec :: forall cs e . (ColorModel cs e, Arbitrary (Color cs e)) => String -> Spec
colorModelSpec name =
  describe "ColorModel" $ do
    toFromComponentsSpec @cs @e
    it "name" $ showsColorModelName (Proxy :: Proxy (Color cs e)) "" `shouldStartWith` name
