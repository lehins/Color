{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Graphics.ColorSpace.CIE1931.IlluminantsSpec (spec) where

import Data.Proxy
import Graphics.ColorSpace.Common
import Graphics.ColorModel.Internal (showsType)
import qualified Data.Colour.CIE as Colour
import qualified Data.Colour.CIE.Illuminant as Colour

shouldMatchApprox ::
     forall i. Illuminant (i :: k)
  => (Double, Double, Double)
  -> WhitePoint i
  -> Spec
shouldMatchApprox (x, y, _) wp =
  let (x', y', z') = (xWhitePoint wp, yWhitePoint wp, zWhitePoint wp)
      eps = 1e-3 -- This is rather unfortunate, but it seems that "colour" package authors
                 -- decided to go with the values from wikipedia, rather than from the
                 -- document created by CIE: Technical Report: Colorimetry, 3rd edition
   in prop (showsType (Proxy :: Proxy (WhitePoint i)) "") $ once $
      epsilonEq eps x x' .&&. epsilonEq eps y y' .&&. epsilonEq 1e-12 1 (x' + y' + z')

spec :: Spec
spec =
  describe "Illuminants" $
    describe "ColourMatch" $ do
      Colour.chromaCoords Colour.a `shouldMatchApprox` (whitePoint :: WhitePoint 'A)
      Colour.chromaCoords Colour.b `shouldMatchApprox` (whitePoint :: WhitePoint 'B)
      Colour.chromaCoords Colour.c `shouldMatchApprox` (whitePoint :: WhitePoint 'C)
      Colour.chromaCoords Colour.d50 `shouldMatchApprox` (whitePoint :: WhitePoint 'D50)
      Colour.chromaCoords Colour.d55 `shouldMatchApprox` (whitePoint :: WhitePoint 'D55)
      Colour.chromaCoords Colour.d65 `shouldMatchApprox` (whitePoint :: WhitePoint 'D65)
      Colour.chromaCoords Colour.d75 `shouldMatchApprox` (whitePoint :: WhitePoint 'D75)
      Colour.chromaCoords Colour.e `shouldMatchApprox` (whitePoint :: WhitePoint 'E)
      Colour.chromaCoords Colour.f1 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL1)
      Colour.chromaCoords Colour.f2 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL2)
      Colour.chromaCoords Colour.f3 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL3)
      Colour.chromaCoords Colour.f4 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL4)
      Colour.chromaCoords Colour.f5 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL5)
      Colour.chromaCoords Colour.f6 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL6)
      Colour.chromaCoords Colour.f7 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL7)
      Colour.chromaCoords Colour.f8 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL8)
      Colour.chromaCoords Colour.f9 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL9)
      Colour.chromaCoords Colour.f10 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL10)
      Colour.chromaCoords Colour.f11 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL11)
      Colour.chromaCoords Colour.f12 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL12)
