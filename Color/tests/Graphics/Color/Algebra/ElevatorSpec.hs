{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Color.Algebra.ElevatorSpec (spec) where

import Data.Int
import Data.Proxy
import Graphics.Color.Algebra
import Graphics.Color.Model.Common

eprop :: (Testable prop, Elevator a, Random a) => String -> (a -> prop) -> Spec
eprop name = prop name . forAll arbitraryElevator

prop_NegativeBecomesPositive :: (Show a, Ord a, Num a) => (b -> a) -> Negative b -> Property
prop_NegativeBecomesPositive f (Negative val) = property $ f val `shouldSatisfy` (>= 0)

specNegativeBecomesPositive :: forall a . (Ord a, Elevator a, Arbitrary a) => Proxy a -> Spec
specNegativeBecomesPositive _ = do
  prop "fromRealFloat :: Float" $ prop_NegativeBecomesPositive (fromRealFloat :: Float -> a)
  prop "fromDouble" $ prop_NegativeBecomesPositive (fromDouble :: Double -> a)
  prop "fromRealFloat :: Double" $ prop_NegativeBecomesPositive (fromRealFloat :: Double -> a)
  prop "toFloat" $ prop_NegativeBecomesPositive (toFloat :: a -> Float)
  prop "toDouble" $ prop_NegativeBecomesPositive (toDouble :: a -> Double)
  prop "toRealFloat :: Float" $ prop_NegativeBecomesPositive (toRealFloat :: a -> Float)
  prop "toRealFloat :: Double" $ prop_NegativeBecomesPositive (toRealFloat :: a -> Double)

prop_NegativeBecomesZero :: (Show a, Ord a, Num a) => (b -> a) -> Negative b -> Property
prop_NegativeBecomesZero f (Negative val) = property $ f val `shouldSatisfy` (== 0)

specNegativeBecomesZero :: forall a . (Ord a, Elevator a) => Proxy a -> Spec
specNegativeBecomesZero _ = do
  prop "fromRealFloat :: Float" $ prop_NegativeBecomesZero (fromRealFloat :: Float -> a)
  prop "fromDouble" $ prop_NegativeBecomesZero (fromDouble :: Double -> a)
  prop "fromRealFloat :: Double" $ prop_NegativeBecomesZero (fromRealFloat :: Double -> a)

spec :: Spec
spec =
  describe "Elevator" $ do
    describe "Word8" $ do
      specNegativeBecomesZero (Proxy :: Proxy Word8)
      eprop "toWord8" $ \(e :: Word8) -> e === toWord8 e
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Word8)
      eprop "toWord8 . toWord16" $ \(e :: Word8) -> e === toWord8 (toWord16 e)
      eprop "toWord8 . toWord32" $ \(e :: Word8) -> e === toWord8 (toWord32 e)
      eprop "toWord8 . toWord64" $ \(e :: Word8) -> e === toWord8 (toWord64 e)
      eprop "toWord8 . toFloat" $ \(e :: Word8) -> e === toWord8 (toFloat e)
      eprop "toWord8 . toDouble" $ \(e :: Word8) -> e === toWord8 (toDouble e)
      eprop "toWord8 . toFloat :: Float" $ \(e :: Word8) -> e === toWord8 (toFloat e :: Float)
      eprop "toWord8 . toRealFloat :: Float" $ \(e :: Word8) ->
        e === toWord8 (toRealFloat e :: Float)
      eprop "toWord8 . toRealFloat :: Double" $ \(e :: Word8) ->
        e === toWord8 (toRealFloat e :: Double)
      eprop "fromRealFloat . toFloat :: Float" $ \(e :: Word8) -> e === fromRealFloat (toFloat e)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Word8) ->
        e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Word8) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Word8) -> e === fromDouble (toDouble e)
      eprop "fromRealFloat . toDouble" $ \(e :: Word8) -> e === fromRealFloat (toDouble e)
      eprop "read . toShowS" $ \(e :: Word8) -> e === read (toShowS e "")
    describe "Word16" $ do
      specNegativeBecomesZero (Proxy :: Proxy Word16)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Word16)
      eprop "toWord16" $ \(e :: Word16) -> e === toWord16 e
      eprop "toWord16 . toWord32" $ \(e :: Word16) -> e === toWord16 (toWord32 e)
      eprop "toWord16 . toWord64" $ \(e :: Word16) -> e === toWord16 (toWord64 e)
      eprop "toWord16 . toFloat" $ \(e :: Word16) -> e === toWord16 (toFloat e)
      eprop "toWord16 . toDouble" $ \(e :: Word16) -> e === toWord16 (toDouble e)
      eprop "toWord16 . toRealFloat :: Float" $ \(e :: Word16) ->
        e === toWord16 (toRealFloat e :: Float)
      eprop "toWord16 . toRealFloat :: Double" $ \(e :: Word16) ->
        e === toWord16 (toRealFloat e :: Double)
      eprop "fromRealFloat . toFloat :: Float" $ \(e :: Word16) -> e === fromRealFloat (toFloat e)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Word16) ->
        e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Word16) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Word16) -> e === fromDouble (toDouble e)
      eprop "fromRealFloat . toDouble" $ \(e :: Word16) -> e === fromRealFloat (toDouble e)
      eprop "read . toShowS" $ \(e :: Word16) -> e === read (toShowS e "")
    describe "Word32" $ do
      specNegativeBecomesZero (Proxy :: Proxy Word32)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Word32)
      eprop "toWord32" $ \(e :: Word32) -> e === toWord32 e
      eprop "toWord32 . toWord64" $ \(e :: Word32) -> e === toWord32 (toWord64 e)
      prop "toWord32 . toFloat" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word32) -> e === toWord32 (toFloat e)
      eprop "toWord32 . toDouble" $ \(e :: Word32) -> e === toWord32 (toDouble e)
      prop "toWord32 . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word32) -> e === toWord32 (toRealFloat e :: Float)
      eprop "toWord32 . toRealFloat :: Double" $ \(e :: Word32) ->
        e === toWord32 (toRealFloat e :: Double)
      prop "fromRealFloat . toFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word32) -> e === fromRealFloat (toFloat e :: Float)
      prop "fromRealFloat . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word32) ->
          e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Word32) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Word32) -> e === fromDouble (toDouble e)
      eprop "fromRealFloat . toDouble" $ \(e :: Word32) -> e === fromRealFloat (toDouble e)
      eprop "read . toShowS" $ \(e :: Word32) -> e === read (toShowS e "")
    describe "Word64" $ do
      specNegativeBecomesZero (Proxy :: Proxy Word64)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Word64)
      eprop "toWord64" $ \(e :: Word64) -> e === toWord64 e
      prop "toWord64 . toFloat" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word64) -> e === toWord64 (toFloat e)
      prop "toWord64 . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word64) ->
          shouldBeApproxIntegral 1 e (toWord64 (toDouble e))
      prop "toWord64 . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word64) -> e === toWord64 (toRealFloat e :: Float)
      prop "toWord64 . toRealFloat :: Double" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word64) ->
          shouldBeApproxIntegral 1 e (toWord64 (toRealFloat e :: Double))
      prop "fromRealFloat . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word64) ->
          e === fromRealFloat (toRealFloat e :: Float)
      prop "fromRealFloat . toRealFloat :: Double" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word64) ->
          e === fromRealFloat (toRealFloat e :: Double)
      prop "fromDouble . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word64) ->
          shouldBeApproxIntegral 1 e (fromDouble (toDouble e))
      prop "fromRealFloat . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word64) ->
          shouldBeApproxIntegral 1 e (fromRealFloat (toDouble e))
      eprop "read . toShowS" $ \(e :: Word64) -> e === read (toShowS e "")
    describe "Word" $ do
      specNegativeBecomesZero (Proxy :: Proxy Word)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Word)
      eprop "toWord64" $ \(e :: Word) -> fromIntegral e === toWord64 e
      prop "toWord64 . toFloat" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word) -> fromIntegral e === toWord64 (toFloat e)
      prop "toWord64 . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word) ->
          shouldBeApproxIntegral 1 (fromIntegral e) (toWord64 (toDouble e))
      prop "toWord64 . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word) ->
          fromIntegral e === toWord64 (toRealFloat e :: Float)
      prop "toWord64 . toRealFloat :: Double" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word) ->
          shouldBeApproxIntegral 1 (fromIntegral e) (toWord64 (toRealFloat e :: Double))
      prop "fromRealFloat . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word) ->
          e === fromRealFloat (toRealFloat e :: Float)
      prop "fromRealFloat . toRealFloat :: Double" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word) ->
          e === fromRealFloat (toRealFloat e :: Double)
      prop "fromDouble . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word) ->
          shouldBeApproxIntegral 1 e (fromDouble (toDouble e))
      prop "fromRealFloat . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Word) ->
          shouldBeApproxIntegral 1 e (fromRealFloat (toDouble e))
      eprop "read . toShowS" $ \(e :: Word) -> e === read (toShowS e "")
    describe "Float" $ do
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Float)
      eprop "fromRealFloat . toFloat" $ \(e :: Float) -> e === fromRealFloat (toFloat e)
      eprop "fromRealFloat . toDouble" $ \(e :: Float) -> e === fromRealFloat (toDouble e)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Float) ->
        e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Float) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Float) -> e === fromDouble (toDouble e)
      eprop "fromRealFloat . toDouble" $ \(e :: Float) -> e === fromRealFloat (toDouble e)
      --eprop "read . toShowS" $ \(e :: Float) -> e === read (toShowS e "")
      it "toWord32 (maxBound edge case)" $ toWord32 (1 :: Float) `shouldBe` maxBound
    describe "Double" $ do
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Double)
      eprop "fromRealFloat . toDouble" $ \(e :: Double) -> e === fromRealFloat (toDouble e)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Double) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Double) -> e === fromDouble (toDouble e)
      eprop "fromRealFloat . toDouble" $ \(e :: Double) -> e === fromRealFloat (toDouble e)
      it "toWord64 (maxBound edge case)" $ toWord64 (1 :: Double) `shouldBe` maxBound
    let pos :: (Integral a, Num b) => a -> b
        pos = fromIntegral . max 0
    describe "Int8" $ do
      specNegativeBecomesPositive (Proxy :: Proxy Int8)
      eprop "fromWord8 . toWord8" $ \(e :: Int8) -> e === fromWord8 (toWord8 e)
      eprop "fromRealFloat . toFloat :: Float" $ \(e :: Int8) ->
        (pos e :: Int8) === fromRealFloat (toFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Int8) ->
        (pos e :: Int8) === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Int8) ->
        (pos e :: Int8) === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Int8) -> (pos e :: Int8) === fromDouble (toDouble e)
      eprop "fromRealFloat . toDouble" $ \(e :: Int8) ->
        (pos e :: Int8) === fromRealFloat (toDouble e)
      eprop "read . toShowS" $ \(e :: Int8) -> e === read (toShowS e "")
    describe "Int16" $ do
      specNegativeBecomesPositive (Proxy :: Proxy Int16)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Int16)
      eprop "fromRealFloat . toFloat :: Float" $ \(e :: Int16) ->
        (pos e :: Int16) === fromRealFloat (toFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Int16) ->
        (pos e :: Int16) === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Int16) ->
        (pos e :: Int16) === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Int16) -> (pos e :: Int16) === fromDouble (toDouble e)
      eprop "fromRealFloat . toDouble" $ \(e :: Int16) ->
        (pos e :: Int16) === fromRealFloat (toDouble e)
      eprop "read . toShowS" $ \(e :: Int16) -> e === read (toShowS e "")
    describe "Int32" $ do
      specNegativeBecomesPositive (Proxy :: Proxy Int32)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Int32)
      prop "fromRealFloat . toFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Int32) ->
          (pos e :: Int32) === fromRealFloat (toFloat e)
      prop "fromRealFloat . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Int32) ->
          (pos e :: Int32) === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Int32) ->
        (pos e :: Int32) === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Int32) -> (pos e :: Int32) === fromDouble (toDouble e)
      eprop "read . toShowS" $ \(e :: Int32) -> e === read (toShowS e "")
    describe "Int64" $ do
      specNegativeBecomesPositive (Proxy :: Proxy Int64)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Int64)
      prop "fromRealFloat . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Int64) ->
          (pos e :: Int64) === fromRealFloat (toRealFloat e :: Float)
      prop "fromRealFloat . toRealFloat :: Double" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Int64) ->
          shouldBeApproxIntegral 1 (pos e :: Int64) (fromRealFloat (toRealFloat e :: Double))
      prop "fromDouble . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Int64) ->
          shouldBeApproxIntegral 1 (pos e) (fromDouble (toDouble e) :: Int64)
      eprop "read . toShowS" $ \(e :: Int64) -> e === read (toShowS e "")
    describe "Int" $ do
      specNegativeBecomesPositive (Proxy :: Proxy Int)
      eprop "toWord8 . fromWord8" $ \(e :: Word8) -> e === toWord8 (fromWord8 e :: Int)
      prop "fromRealFloat . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Int) ->
          (pos e :: Int) === fromRealFloat (toRealFloat e :: Float)
      prop "fromRealFloat . toRealFloat :: Double" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Int) ->
          shouldBeApproxIntegral 1 (pos e :: Int) (fromRealFloat (toRealFloat e :: Double))
      prop "fromDouble . toDouble" $
        forAll (choose (0, maxDoubleI)) $ \(e :: Int) ->
          shouldBeApproxIntegral 1 (pos e) (fromDouble (toDouble e) :: Int)
      eprop "read . toShowS" $ \(e :: Int) -> e === read (toShowS e "")
  where
    maxFloatI, maxDoubleI :: Integral a => a
    maxFloatI = 2 ^ (24 :: Int)
    maxDoubleI = 2 ^ (54 :: Int)
