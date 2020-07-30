{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Illuminant.Common
  ( illuminantSpec
  ) where

import Data.Proxy
import Graphics.Color.Space
import Graphics.Color.Model.Common
import GHC.TypeLits
import Graphics.Color.Algebra (showsType)


colorTemperatureExpectation ::
     forall i. Illuminant i
  => Expectation
colorTemperatureExpectation =
  round (unCCT (colorTemperature :: CCT i)) `shouldBe` natVal (Proxy :: Proxy (Temperature i))


illuminantSpec ::
     forall i. (Illuminant i)
  => Spec
illuminantSpec =
  let wp = whitePoint :: WhitePoint i Double
  in describe (showsType (Proxy :: Proxy i) "") $ do
       it "colorTemperature" $ colorTemperatureExpectation @i
       it "tristimulus to chromaticity" $
         fromColorXYZ (whitePointTristimulus :: Color (XYZ i) Double) `epsilonEqColorDouble`
           case wp of
             WhitePointChromaticity (Chromaticity c) -> c
       prop "whitePointXZ" $ forAll arbitraryElevator $ \ vY ->
         sum (whitePointXZ vY wp) `epsilonEqDouble` vY / yWhitePoint wp
