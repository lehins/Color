{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Illuminant.CIE1931Spec (spec) where

import Graphics.Color.Space.Common
import Graphics.Color.Illuminant.Common
import Graphics.Color.Illuminant.CIE1931

spec :: Spec
spec =
  describe "Wikipedia" $ do
    describe "IlluminantSpec" $ do
      illuminantSpec @'A
      illuminantSpec @'B
      illuminantSpec @'C
      illuminantSpec @'D50
      illuminantSpec @'D55
      illuminantSpec @'D60
      illuminantSpec @'D65
      illuminantSpec @'D75
      illuminantSpec @'E
      illuminantSpec @'FL1
      illuminantSpec @'FL2
      illuminantSpec @'FL3
      illuminantSpec @'FL4
      illuminantSpec @'FL5
      illuminantSpec @'FL6
      illuminantSpec @'FL7
      illuminantSpec @'FL8
      illuminantSpec @'FL9
      illuminantSpec @'FL10
      illuminantSpec @'FL11
      illuminantSpec @'FL12
      illuminantSpec @'FL3_1
      illuminantSpec @'FL3_2
      illuminantSpec @'FL3_3
      illuminantSpec @'FL3_4
      illuminantSpec @'FL3_5
      illuminantSpec @'FL3_6
      illuminantSpec @'FL3_7
      illuminantSpec @'FL3_8
      illuminantSpec @'FL3_9
      illuminantSpec @'FL3_10
      illuminantSpec @'FL3_11
      illuminantSpec @'FL3_12
      illuminantSpec @'FL3_13
      illuminantSpec @'FL3_14
      illuminantSpec @'FL3_15
      illuminantSpec @'HP1
      illuminantSpec @'HP2
      illuminantSpec @'HP3
      illuminantSpec @'HP4
      illuminantSpec @'HP5
    describe "Derived Classes" $ do
      let is = [A .. HP5]
      it "Bounded" $ [minBound .. maxBound] `shouldBe` is
      it "Enum" $ forM_ is $ \ i -> toEnum (fromEnum i) `shouldBe` i
      it "Read . Show" $ forM_ is $ \ i -> read (show i) `shouldBe` i
      it "Read . Show" $  read (show is) `shouldBe` (is :: [CIE1931])
