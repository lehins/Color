{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Graphics.Color.Illuminant.WikipediaSpec (spec) where

import Data.Proxy
import Graphics.Color.Space.Common
import Graphics.Color.Illuminant.Wikipedia
import qualified Data.Colour.CIE as Colour
import qualified Data.Colour.CIE.Illuminant as Colour

shouldMatch ::
     forall i. Illuminant i
  => (Double, Double, Double)
  -> WhitePoint i Double
  -> Spec
shouldMatch (x, y, _) wp =
  let (x', y', z') = (xWhitePoint wp, yWhitePoint wp, zWhitePoint wp)
   in it (showsType (Proxy :: Proxy (WhitePoint i Double)) "") $ do
        x `shouldBe` x'
        y `shouldBe` y'
        epsilonExpect 1e-15 1 (x' + y' + z')

instance Arbitrary Degree2 where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec =
  describe "Wikipedia" $ do
    describe "Match 'colour' package" $ do
      Colour.chromaCoords Colour.a `shouldMatch` (whitePoint :: WhitePoint 'A Double)
      Colour.chromaCoords Colour.b `shouldMatch` (whitePoint :: WhitePoint 'B Double)
      Colour.chromaCoords Colour.c `shouldMatch` (whitePoint :: WhitePoint 'C Double)
      Colour.chromaCoords Colour.d50 `shouldMatch` (whitePoint :: WhitePoint 'D50 Double)
      Colour.chromaCoords Colour.d55 `shouldMatch` (whitePoint :: WhitePoint 'D55 Double)
      Colour.chromaCoords Colour.d65 `shouldMatch` (whitePoint :: WhitePoint 'D65 Double)
      Colour.chromaCoords Colour.d75 `shouldMatch` (whitePoint :: WhitePoint 'D75 Double)
      Colour.chromaCoords Colour.e `shouldMatch` (whitePoint :: WhitePoint 'E Double)
      Colour.chromaCoords Colour.f1 `shouldMatch` (whitePoint :: WhitePoint 'F1 Double)
      Colour.chromaCoords Colour.f2 `shouldMatch` (whitePoint :: WhitePoint 'F2 Double)
      Colour.chromaCoords Colour.f3 `shouldMatch` (whitePoint :: WhitePoint 'F3 Double)
      Colour.chromaCoords Colour.f4 `shouldMatch` (whitePoint :: WhitePoint 'F4 Double)
      Colour.chromaCoords Colour.f5 `shouldMatch` (whitePoint :: WhitePoint 'F5 Double)
      Colour.chromaCoords Colour.f6 `shouldMatch` (whitePoint :: WhitePoint 'F6 Double)
      Colour.chromaCoords Colour.f7 `shouldMatch` (whitePoint :: WhitePoint 'F7 Double)
      Colour.chromaCoords Colour.f8 `shouldMatch` (whitePoint :: WhitePoint 'F8 Double)
      Colour.chromaCoords Colour.f9 `shouldMatch` (whitePoint :: WhitePoint 'F9 Double)
      Colour.chromaCoords Colour.f10 `shouldMatch` (whitePoint :: WhitePoint 'F10 Double)
      Colour.chromaCoords Colour.f11 `shouldMatch` (whitePoint :: WhitePoint 'F11 Double)
      Colour.chromaCoords Colour.f12 `shouldMatch` (whitePoint :: WhitePoint 'F12 Double)
    describe "Derived Classes" $ do
      it "Bounded" $ [minBound .. maxBound] `shouldBe` [A .. F12]
      prop "Enum" $ \ (i :: Degree2) -> toEnum (fromEnum i) === i
      prop "Read . Show" $ \ (i :: Degree2) -> read (show i) === i
      prop "Read . Show" $ \ is -> read (show is) === (is :: [Degree2])
