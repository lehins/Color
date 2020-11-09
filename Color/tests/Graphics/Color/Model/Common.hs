{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Model.Common
  ( colorModelSpec
  , toFromComponentsSpec
  , izipWithM_
  , matchListsWith
  , expectSameLength
  , epsilonExpect
  , epsilonFoldableExpect
  , epsilonColorExpect
  , epsilonColorIxSpec
  , epsilonEq
  , epsilonEqFloat
  , epsilonEqDouble
  , epsilonEqColor
  , epsilonEqColorFloat
  , epsilonEqColorDouble
  , epsilonEqColorTol
  , epsilonEqColorTolIx
  -- * Integral
  , shouldBeApproxIntegral
  , approxIntegralColorExpect
  , approxIntegralColorExpect1
  , arbitraryElevator
  , module Test.Hspec
  , module Test.Hspec.QuickCheck
  , module Test.QuickCheck
  , module System.Random
  , module F
  ) where

import Prelude as P
import Control.Monad as M
import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Proxy
import Graphics.Color.Model
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit (assertBool)
import Test.Massiv.Array.Mutable
import Test.QuickCheck
import GHC.TypeLits

instance (Random e, ColorModel cs e, Arbitrary (Color cs e)) => Arbitrary (Color (Alpha cs) e) where
  arbitrary = addAlpha <$> arbitrary <*> arbitraryElevator

instance (ColorModel cs e, CoArbitrary (Components cs e)) => CoArbitrary (Color cs e) where
  coarbitrary c = coarbitrary (toComponents c)

instance (Function (Components cs e), ColorModel cs e) => Function (Color cs e) where
  function = functionMap toComponents fromComponents

infix 1 `epsilonEqFloat`, `epsilonEqDouble`, `approxIntegralColorExpect1`

izipWithM_ :: Applicative m => (Int -> a -> b -> m c) -> [a] -> [b] -> m ()
izipWithM_ f xs = zipWithM_ (uncurry f) (P.zip [0..] xs)


-- | Match to lists exactly element-by-element with an expectation.
matchListsWith :: (a -> b -> Expectation) -> [a] -> [b] -> Expectation
matchListsWith f xs ys = do
  expectSameLength xs ys
  zipWithM_ f xs ys

expectSameLength :: Foldable t => t a1 -> t a2 -> IO ()
expectSameLength xs ys =
  unless (length xs == length ys) $
  expectationFailure $ "List lengths mismatch: " ++ show (length xs) ++ "/=" ++ show (length ys)


arbitraryElevator :: (Elevator e, Random e) => Gen e
arbitraryElevator = choose (minValue, maxValue)

shouldBeApproxIntegral ::
     (HasCallStack, Show a, Integral a)
  => Word8 -- ^ Epsilon, a maximum tolerated error.
  -> a -- ^ Tested value.
  -> a -- ^ Expected result.
  -> Expectation
shouldBeApproxIntegral epsilon result expected
  | result == expected = pure ()
  | otherwise =
    assertBool
      (concat
         [show result, " /= ", show expected, " (Tolerance: ", show diff, " > ", show epsilon, ")"])
      (diff <= fromIntegral epsilon)
  where
    diff
      | result > expected = result - expected
      | otherwise = expected - result

approxIntegralColorExpect ::
     (HasCallStack, ColorModel cs e, Integral e) => Word8 -> Color cs e -> Color cs e -> Expectation
approxIntegralColorExpect epsilon x y =
  zipWithM_ (shouldBeApproxIntegral epsilon) (F.toList x) (F.toList y)

approxIntegralColorExpect1 ::
     (HasCallStack, ColorModel cs e, Integral e) => Color cs e -> Color cs e -> Expectation
approxIntegralColorExpect1 = approxIntegralColorExpect 1

epsilonExpect ::
     (HasCallStack, Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Expectation
epsilonExpect epsilon x y =
  M.forM_ (epsilonMaybeEq epsilon x y) $ \errMsg ->
    expectationFailure $ "Expected: " ++ show x ++ " but got: " ++ show y ++ "\n   " ++ errMsg


epsilonMaybeEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Maybe String
epsilonMaybeEq epsilon x y
  | isNaN x && not (isNaN y) = Just $ "Expected NaN, but got: " ++ show y
  | x == y = Nothing
  | diff > n = Just $ concat [show x, " /= ", show y, " (Tolerance: ", show diff, " > ", show n, ")"]
  | otherwise = Nothing
  where
    (absx, absy) = (abs x, abs y)
    n = epsilon * (1 + max absx absy)
    diff = abs (y - x)

epsilonColorExpect ::
     (HasCallStack, ColorModel cs e, RealFloat e) => e -> Color cs e -> Color cs e -> Expectation
epsilonColorExpect = epsilonFoldableExpect

epsilonFoldableExpect ::
     (HasCallStack, Foldable f, Show (f e), Show e, RealFloat e) => e -> f e -> f e -> Expectation
epsilonFoldableExpect epsilon x y =
  M.forM_ (zipWithM (epsilonMaybeEq epsilon) (F.toList x) (F.toList y)) $ \errMsgs ->
    expectationFailure $
    "Expected: " ++ show x ++ " but got: " ++ show y ++ "\n" ++ unlines (P.map ("    " ++) errMsgs)

epsilonColorIxSpec ::
     (HasCallStack, ColorModel cs e, RealFloat e)
  => e
  -> Int
  -> Color cs e
  -> Color cs e
  -> Spec
epsilonColorIxSpec epsilon ix x y =
  it ("Index: " ++ show ix) $ epsilonColorExpect epsilon x y


epsilonEq ::
     (Show a, RealFloat a)
  => a -- ^ Epsilon, a maximum tolerated error. Sign is ignored.
  -> a -- ^ Expected result.
  -> a -- ^ Tested value.
  -> Property
epsilonEq epsilon x y = property $ epsilonExpect epsilon x y

epsilonEqDouble ::
     Double -- ^ Expected result.
  -> Double -- ^ Tested value.
  -> Property
epsilonEqDouble = epsilonEq epsilon
  where
    epsilon = 1e-12

epsilonEqFloat ::
     Float -- ^ Expected result.
  -> Float -- ^ Tested value.
  -> Property
epsilonEqFloat = epsilonEq epsilon
  where
    epsilon = 1e-6


epsilonEqColor :: (ColorModel cs e, RealFloat e) => Color cs e -> Color cs e -> Property
epsilonEqColor = epsilonEqColorTol epsilon
  where
    epsilon = 1e-11

epsilonEqColorDouble :: ColorModel cs Double => Color cs Double -> Color cs Double -> Property
epsilonEqColorDouble = epsilonEqColorTol epsilon
  where
    epsilon = 1e-12

epsilonEqColorFloat :: ColorModel cs Float => Color cs Float -> Color cs Float -> Property
epsilonEqColorFloat = epsilonEqColorTol epsilon
  where
    epsilon = 1e-6

epsilonEqColorTol :: (ColorModel cs e, RealFloat e) => e -> Color cs e -> Color cs e -> Property
epsilonEqColorTol epsilon x y = property $ epsilonColorExpect epsilon x y
  --conjoin $ F.toList $ liftA2 (epsilonEq epsilon) x y

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

colorModelSpec ::
     forall cs e.
     ( ColorModel cs e
     , Function (Components cs e)
     , CoArbitrary (Components cs e)
     , Arbitrary (Color cs e)
     , KnownNat (ChannelCount cs)
     )
  => String
  -> Spec
colorModelSpec name =
  describe "ColorModel" $ do
    toFromComponentsSpec @cs @e
    it "Model Name" $ showsColorModelName (Proxy :: Proxy (Color cs e)) "" `shouldStartWith` name
    it "ChannelCount" $ do
      let px = Proxy :: Proxy (Color cs e)
          count = channelCount px
      count `shouldBe` fromInteger (natVal (Proxy :: Proxy (ChannelCount cs)))
      length (channelNames px)  `shouldBe` count
      length (channelColors px)  `shouldBe` count
    modifyMaxSuccess (`div` 10) $ describe "Array" $ do
      describe "Storable" $
        mutableSpec @S @Ix1 @(Color cs e)
      describe "Unboxed" $
        mutableSpec @U @Ix1 @(Color cs e)
