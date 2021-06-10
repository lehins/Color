{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Color.Standard.Internal
-- Copyright   : (c) Alexey Kuleshevich 2019-2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Standard.Internal
  ( StandardColor(..)
  ) where

import Graphics.Color.Space


-- | Get a specific colors from a standard:
--
-- >>> import Graphics.Color.Standard
-- >>> color (RAL :: RAL 9003) :: Color (SRGB 'NonLinear) Float
-- <SRGB 'NonLinear:( 0.92477065, 0.92470974, 0.90498060)>
-- >>> color (RAL :: RAL "Signal white") :: Color (SRGB 'NonLinear) Float
-- <SRGB 'NonLinear:( 0.92477065, 0.92470974, 0.90498060)>
--
class StandardColor std (code :: k) where
  color :: ColorSpace cs i e => std code -> Color cs e
