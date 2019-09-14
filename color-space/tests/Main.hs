module Main where

import Spec
import System.IO    (BufferMode (LineBuffering), hSetBuffering, stdout)
import Test.DocTest
import Test.Hspec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "\n=== Running HSpec ==="
  hspec spec
  putStrLn "\n=== Running DocTests ==="
  doctest ["src"]
