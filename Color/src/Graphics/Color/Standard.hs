{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Color.Standard
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Standard
  ( SVG(..)
  , RAL(..)
  , StandardColor(..)
  ) where
import Graphics.Color.Standard.Internal
import Graphics.Color.Standard.RAL (RAL(..))
import Graphics.Color.Standard.SVG (SVG(..))
