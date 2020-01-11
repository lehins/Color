{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Graphics.Color.Space.CIE1931.IlluminantSpec (spec) where

import Data.Proxy
import Graphics.Color.Space.Common
import Graphics.Color.Illuminant.CIE1931 as CIE1931
import qualified Data.Colour.CIE as Colour
import qualified Data.Colour.CIE.Illuminant as Colour

shouldMatchApprox ::
     forall i. Illuminant (i :: k)
  => (Double, Double, Double)
  -> WhitePoint i Double
  -> Spec
shouldMatchApprox (x, y, _) wp =
  let (x', y', z') = (xWhitePoint wp, yWhitePoint wp, zWhitePoint wp)
      eps = 1e-3 -- This is rather unfortunate, but it seems that "colour" package authors
                 -- decided to go with the values from wikipedia, rather than from the
                 -- document created by CIE: Technical Report: Colorimetry, 3rd edition
   in prop (showsType (Proxy :: Proxy (WhitePoint i Double)) "") $ once $
      epsilonEq eps x x' .&&. epsilonEq eps y y' .&&. epsilonEq 1e-12 1 (x' + y' + z')

spec :: Spec
spec =
  describe "Illuminants" $
    describe "ColourMatch" $ do
      Colour.chromaCoords Colour.a `shouldMatchApprox` (whitePoint :: WhitePoint 'A Double)
      Colour.chromaCoords Colour.b `shouldMatchApprox` (whitePoint :: WhitePoint 'B Double)
      Colour.chromaCoords Colour.c `shouldMatchApprox` (whitePoint :: WhitePoint 'C Double)
      Colour.chromaCoords Colour.d50 `shouldMatchApprox` (whitePoint :: WhitePoint 'D50 Double)
      Colour.chromaCoords Colour.d55 `shouldMatchApprox` (whitePoint :: WhitePoint 'D55 Double)
      Colour.chromaCoords Colour.d65 `shouldMatchApprox` (whitePoint :: WhitePoint 'CIE1931.D65 Double)
      Colour.chromaCoords Colour.d75 `shouldMatchApprox` (whitePoint :: WhitePoint 'D75 Double)
      Colour.chromaCoords Colour.e `shouldMatchApprox` (whitePoint :: WhitePoint 'E Double)
      Colour.chromaCoords Colour.f1 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL1 Double)
      Colour.chromaCoords Colour.f2 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL2 Double)
      Colour.chromaCoords Colour.f3 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL3 Double)
      Colour.chromaCoords Colour.f4 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL4 Double)
      Colour.chromaCoords Colour.f5 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL5 Double)
      Colour.chromaCoords Colour.f6 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL6 Double)
      Colour.chromaCoords Colour.f7 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL7 Double)
      Colour.chromaCoords Colour.f8 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL8 Double)
      Colour.chromaCoords Colour.f9 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL9 Double)
      Colour.chromaCoords Colour.f10 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL10 Double)
      Colour.chromaCoords Colour.f11 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL11 Double)
      Colour.chromaCoords Colour.f12 `shouldMatchApprox` (whitePoint :: WhitePoint 'FL12 Double)
