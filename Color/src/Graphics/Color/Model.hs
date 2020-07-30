-- |
-- Module      : Graphics.Color.Model
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Model
  ( ColorModel(..)
  -- * Alpha
  , Alpha
  , Opaque
  , addAlpha
  , getAlpha
  , setAlpha
  , dropAlpha
  , modifyAlpha
  , modifyOpaque
  -- * X
  , module Graphics.Color.Model.X
  -- * RGB
  , module Graphics.Color.Model.RGB
  -- * HSI
  , module Graphics.Color.Model.HSI
  -- * HSL
  , module Graphics.Color.Model.HSL
  -- * HSV
  , module Graphics.Color.Model.HSV
  -- * YCbCr
  , module Graphics.Color.Model.YCbCr
  -- * CMYK
  , module Graphics.Color.Model.CMYK
  -- * Color
  , Color(..)
  , module Graphics.Color.Algebra.Binary
  , module Graphics.Color.Algebra.Elevator
  ) where

import Graphics.Color.Algebra.Binary
import Graphics.Color.Algebra.Elevator
import Graphics.Color.Model.CMYK
import Graphics.Color.Model.HSI
import Graphics.Color.Model.HSL
import Graphics.Color.Model.HSV
import Graphics.Color.Model.Internal
import Graphics.Color.Model.RGB
import Graphics.Color.Model.X
import Graphics.Color.Model.YCbCr
