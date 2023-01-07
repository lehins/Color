{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Criterion.Main
import Control.DeepSeq
import qualified Graphics.Color.Model as CM
import Graphics.Color.Space
import Graphics.Color.Space.RGB.ITU.Rec601
import Graphics.Color.Space.RGB.ITU.Rec709
import Data.Coerce

main :: IO ()
main = do
  defaultMain
    [ bgroup
        "toYCbCr"
        [ toYCbCrBench (CM.ColorRGB 0.1 0.2 0.3 :: Color CM.RGB Float) "Float"
        , toYCbCrBench (CM.ColorRGB 0.1 0.2 0.3 :: Color CM.RGB Double) "Double"
        ]
    , bgroup
        "fromYCbCr"
        [ fromYCbCrBench (CM.ColorYCbCr 0.1 0.2 0.3 :: Color CM.YCbCr Float) "Float"
        , fromYCbCrBench (CM.ColorYCbCr 0.1 0.2 0.3 :: Color CM.YCbCr Double) "Double"
        ]
    ]


toYCbCrBench ::
     forall e. (Elevator e, NFData e)
  => Color CM.RGB e
  -> String
  -> Benchmark
toYCbCrBench rgb tyName =
  bgroup
    tyName
    [ bgroup
        "Standard"
        [ bench "SRGB" $
          nf (fromBaseSpace :: Color (SRGB 'NonLinear) e -> Color (Y'CbCr SRGB) e) (mkColorRGB rgb)
        , bench "Rec601" $
          nf
            (fromBaseSpace :: Color (BT601_625 'NonLinear) e -> Color (Y'CbCr BT601_625) e)
            (mkColorRGB rgb)
        , bench "Rec709" $
          nf (fromBaseSpace :: Color (BT709 'NonLinear) e -> Color (Y'CbCr BT709) e) (mkColorRGB rgb)
        ]
    ]

fromYCbCrBench ::
     forall e. (Elevator e, NFData e)
  => Color CM.YCbCr e
  -> String
  -> Benchmark
fromYCbCrBench ycbcr tyName =
  bgroup
    tyName
    [ bgroup
        "Standard"
        [ bench "SRGB" $
          nf (toBaseSpace :: Color (Y'CbCr SRGB) e -> Color (SRGB 'NonLinear) e) (coerce ycbcr)
        , bench "Rec601" $
          nf
            (toBaseSpace :: Color (Y'CbCr BT601_625) e -> Color (BT601_625 'NonLinear) e)
            (coerce ycbcr)
        , bench "Rec709" $
          nf (toBaseSpace :: Color (Y'CbCr BT709) e -> Color (BT709 'NonLinear) e) (coerce ycbcr)
        ]
    ]
