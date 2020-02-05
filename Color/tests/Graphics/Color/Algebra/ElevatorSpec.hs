{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Algebra.ElevatorSpec (spec) where

import Graphics.Color.Algebra
import Graphics.Color.Model.Common

eprop :: (Testable prop, Elevator a, Random a) => String -> (a -> prop) -> Spec
eprop name = prop name . forAll arbitraryElevator


spec :: Spec
spec =
  describe "Elevator" $ do
    describe "Word8" $ do
      eprop "toWord8 . toWord8" $ \(e :: Word8) -> e === toWord8 e
      eprop "toWord8 . toWord16" $ \(e :: Word8) -> e === toWord8 (toWord16 e)
      eprop "toWord8 . toWord32" $ \(e :: Word8) -> e === toWord8 (toWord32 e)
      eprop "toWord8 . toWord64" $ \(e :: Word8) -> e === toWord8 (toWord64 e)
      eprop "toWord8 . toFloat" $ \(e :: Word8) -> e === toWord8 (toFloat e)
      eprop "toWord8 . toDouble" $ \(e :: Word8) -> e === toWord8 (toDouble e)
      eprop "toWord8 . toRealFloat :: Float" $ \(e :: Word8) ->
        e === toWord8 (toRealFloat e :: Float)
      eprop "toWord8 . toRealFloat :: Double" $ \(e :: Word8) ->
        e === toWord8 (toRealFloat e :: Double)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Word8) ->
        e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Word8) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Word8) -> e === fromDouble (toDouble e)
      eprop "read . toShowS" $ \(e :: Word8) -> e === read (toShowS e "")
    describe "Word16" $ do
      eprop "toWord16 . toWord16" $ \(e :: Word16) -> e === toWord16 e
      eprop "toWord16 . toWord32" $ \(e :: Word16) -> e === toWord16 (toWord32 e)
      eprop "toWord16 . toWord64" $ \(e :: Word16) -> e === toWord16 (toWord64 e)
      eprop "toWord16 . toFloat" $ \(e :: Word16) -> e === toWord16 (toFloat e)
      eprop "toWord16 . toDouble" $ \(e :: Word16) -> e === toWord16 (toDouble e)
      eprop "toWord16 . toRealFloat :: Float" $ \(e :: Word16) ->
        e === toWord16 (toRealFloat e :: Float)
      eprop "toWord16 . toRealFloat :: Double" $ \(e :: Word16) ->
        e === toWord16 (toRealFloat e :: Double)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Word16) ->
        e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Word16) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Word16) -> e === fromDouble (toDouble e)
      eprop "read . toShowS" $ \(e :: Word16) -> e === read (toShowS e "")
    describe "Word32" $ do
      eprop "toWord32 . toWord32" $ \(e :: Word32) -> e === toWord32 e
      eprop "toWord32 . toWord64" $ \(e :: Word32) -> e === toWord32 (toWord64 e)
      prop "toWord32 . toFloat" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word32) -> e === toWord32 (toFloat e)
      eprop "toWord32 . toDouble" $ \(e :: Word32) -> e === toWord32 (toDouble e)
      prop "toWord32 . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word32) -> e === toWord32 (toRealFloat e :: Float)
      eprop "toWord32 . toRealFloat :: Double" $ \(e :: Word32) ->
        e === toWord32 (toRealFloat e :: Double)
      prop "fromRealFloat . toRealFloat :: Float" $
        forAll (choose (0, maxFloatI)) $ \(e :: Word32) ->
          e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Word32) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Word32) -> e === fromDouble (toDouble e)
      eprop "read . toShowS" $ \(e :: Word32) -> e === read (toShowS e "")
    describe "Word64" $ do
      eprop "toWord64 . toWord64" $ \(e :: Word64) -> e === toWord64 e
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
      eprop "read . toShowS" $ \(e :: Word64) -> e === read (toShowS e "")
    describe "Float" $ do
      -- eprop "toWord8 . toWord8" $ \(e :: Word8) -> e === toWord8 e
      -- eprop "toWord8 . toWord16" $ \(e :: Word8) -> e === toWord8 (toWord16 e)
      -- eprop "toWord8 . toWord32" $ \(e :: Word8) -> e === toWord8 (toWord32 e)
      -- eprop "toWord8 . toWord64" $ \(e :: Word8) -> e === toWord8 (toWord64 e)
      -- eprop "toWord8 . toDouble" $ \(e :: Word8) -> e === toWord8 (toDouble e)
      -- eprop "toWord8 . toRealFloat :: Float" $ \(e :: Word8) ->
      --   e === toWord8 (toRealFloat e :: Float)
      -- eprop "toWord8 . toRealFloat :: Double" $ \(e :: Word8) ->
      --   e === toWord8 (toRealFloat e :: Double)
      eprop "fromRealFloat . toFloat" $ \(e :: Float) -> e === toRealFloat (toFloat e)
      eprop "fromRealFloat . toDouble" $ \(e :: Float) -> e === toRealFloat (toDouble e)
      eprop "fromRealFloat . toRealFloat :: Float" $ \(e :: Float) ->
        e === fromRealFloat (toRealFloat e :: Float)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Float) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Float) -> e === fromDouble (toDouble e)
      --eprop "read . toShowS" $ \(e :: Float) -> e === read (toShowS e "")
      it "toWord32 (maxBound edge case)" $ toWord32 (1 :: Float) `shouldBe` maxBound
    describe "Double" $ do
      eprop "fromRealFloat . toDouble" $ \(e :: Double) -> e === toRealFloat (toDouble e)
      eprop "fromRealFloat . toRealFloat :: Double" $ \(e :: Double) ->
        e === fromRealFloat (toRealFloat e :: Double)
      eprop "fromDouble . toDouble" $ \(e :: Double) -> e === fromDouble (toDouble e)
      it "fromDouble . toWord64" $ toWord64 (1 :: Double) `shouldBe` maxBound
      it "toWord64 (maxBound edge case)" $ toWord64 (1 :: Double) `shouldBe` maxBound
  where
    maxFloatI, maxDoubleI :: Integral a => a
    maxFloatI = 2 ^ (24 :: Int)
    maxDoubleI = 2 ^ (54 :: Int)
