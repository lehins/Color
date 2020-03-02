{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.Color.Space
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space
  ( convertColor
  , convertColorFloat
  , module X
  ) where

import Graphics.Color.Model.Internal as X
import Graphics.Color.Space.Internal as X
import Graphics.Color.Space.RGB.Internal as X


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
