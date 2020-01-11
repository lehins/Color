{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Space.RGB.VGamut
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Space.RGB.VGamut
  (   ) where

-- vGamutNPM = M3x3 (V3  0.679644  0.152211  0.118600)
--                  (V3  0.260686  0.774894 -0.035580)
--                  (V3 -0.009310 -0.004612  1.102980)

-- vGamutINPM = M3x3 (V3  1.589012 -0.313204 -0.180965)
--                   (V3 -0.534053  1.396011  0.102458)
--                   (V3  0.011179  0.003194  0.905535)

-- acesNPM = M3x3 (V3 0.9525523959 0.0000000000  0.0000936786)
--                (V3 0.3439664498 0.7281660966 -0.0721325464)
--                (V3 0.0000000000 0.0000000000  1.0088251844)

-- acesINPM = M3x3 (V3  1.0498110175 0.0000000000 -0.0000974845)
--                 (V3 -0.4959030231 1.3733130458  0.0982400361)
--                 (V3  0.0000000000 0.0000000000  0.9912520182)
