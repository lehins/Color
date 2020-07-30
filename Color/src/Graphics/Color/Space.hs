{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Color.Space
-- Copyright   : (c) Alexey Kuleshevich 2018-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space
  ( convertColor
  , convertColorFloat
  , module Graphics.Color.Space.Internal
  , module Graphics.Color.Space.RGB.Internal
  -- , module Graphics.Color.Space.RGB.SRGB
  , module Graphics.Color.Space.CIE1976.LAB
  ) where

import Graphics.Color.Space.CIE1976.LAB
import Graphics.Color.Space.Internal
import Graphics.Color.Space.RGB.Internal
-- import Graphics.Color.Space.RGB.SRGB


-- | Convert a color space through `XYZ` intermediary with `Double` precision. Illuminant is
-- enforced to be the same, but in case that it is a limitation and chromatic adaptation is
-- needed `Graphics.Color.Adaptation.convertWith` can be used instead.
--
-- @since 0.1.1
convertColor ::
     forall cs cs' i e. (ColorSpace cs' i e, ColorSpace cs i e)
  => Color cs' e
  -> Color cs e
convertColor = fromColorXYZ . (toColorXYZ :: Color cs' e -> Color (XYZ i) Double)
{-# INLINE convertColor #-}

-- | Same as `convertColor`, but use 32bit `Float` as an intermediary precision
--
-- @since 0.1.1
convertColorFloat ::
     forall cs cs' i e. (ColorSpace cs' i e, ColorSpace cs i e)
  => Color cs' e
  -> Color cs e
convertColorFloat = fromColorXYZ . (toColorXYZ :: Color cs' e -> Color (XYZ i) Float)
{-# INLINE convertColorFloat #-}
