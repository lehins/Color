-- |
-- Module      : Graphics.Color.Model
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model
  ( ColorModel(..)
  , Elevator(..)
  , Color
  , Alpha
  , Opaque
  , addAlpha
  , getAlpha
  , setAlpha
  , dropAlpha
  , modifyOpaque
  ) where

import Graphics.Color.Model.Alpha
import Graphics.Color.Model.Internal
