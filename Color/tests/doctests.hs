{-# LANGUAGE CPP #-}
module Main where

#if __GLASGOW_HASKELL__ >= 802

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["src"]

#else

main :: IO ()
main = putStrLn "\nDoctests are not supported for older ghc version\n"

#endif
