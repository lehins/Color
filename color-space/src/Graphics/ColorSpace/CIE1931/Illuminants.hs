{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.ColorSpace.Internal
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.CIE1931.Illuminants
  ( Illuminant2(..)
  , ACES(..)
  ) where

import Graphics.ColorSpace.Internal (Illuminant(..), WhitePoint(..))

instance Illuminant 'A   where whitePoint = WhitePoint 0.44758 0.40745
instance Illuminant 'B   where whitePoint = WhitePoint 0.34842 0.35161
instance Illuminant 'C   where whitePoint = WhitePoint 0.31006 0.31616
instance Illuminant 'D50 where whitePoint = WhitePoint 0.34570 0.35850
instance Illuminant 'D55 where whitePoint = WhitePoint 0.33243 0.34744
instance Illuminant 'D60 where whitePoint = WhitePoint 0.32163 0.33774
instance Illuminant 'D65 where whitePoint = WhitePoint 0.31270 0.32900
instance Illuminant 'D75 where whitePoint = WhitePoint 0.29903 0.31488
instance Illuminant 'E   where whitePoint = WhitePoint (1 / 3) (1 / 3)
instance Illuminant 'F1  where whitePoint = WhitePoint 0.31310 0.33710
instance Illuminant 'F2  where whitePoint = WhitePoint 0.37210 0.37510
instance Illuminant 'F3  where whitePoint = WhitePoint 0.40910 0.39410
instance Illuminant 'F4  where whitePoint = WhitePoint 0.44020 0.40310
instance Illuminant 'F5  where whitePoint = WhitePoint 0.31380 0.34520
instance Illuminant 'F6  where whitePoint = WhitePoint 0.37790 0.38820
instance Illuminant 'F7  where whitePoint = WhitePoint 0.31290 0.32920
instance Illuminant 'F8  where whitePoint = WhitePoint 0.34580 0.35860
instance Illuminant 'F9  where whitePoint = WhitePoint 0.37410 0.37270
instance Illuminant 'F10 where whitePoint = WhitePoint 0.34580 0.35880
instance Illuminant 'F11 where whitePoint = WhitePoint 0.38050 0.37690
instance Illuminant 'F12 where whitePoint = WhitePoint 0.43700 0.40420


-- References:
--
-- * https://web.archive.org/web/20050523033826/http://www.hunterlab.com:80/appnotes/an05_05.pdf
data Illuminant2
  = A
  -- ^ Incandescent / Tungsten
  | B
  -- ^ Direct sunlight at noon (obsolete)
  | C
  -- ^ Average / North sky Daylight (obsolete)
  | D50
  -- ^  Horizon Light.
  | D55
  -- ^ Mid-morning / Mid-afternoon Daylight
  | D60
  | D65
  -- ^ Noon Daylight
  | D75
  -- ^ Overcast dayligh / North sky Daylight
  | E
  -- ^ Equal energy
  | F1
  | F2
  -- ^ The fluorescent illuminant in most common use, represents cool white fluorescent
  -- (4100° Kelvin, CRI 60). Non-standard names include F, F02, Fcw, CWF, CWF2.
  --
  -- /Note/ - Takes precedence over other F illuminants
  | F3
  | F4
  | F5
  | F6
  | F7
  -- ^ Represents a broadband fluorescent lamp, which approximates CIE illuminant D65
  -- (6500° Kelvin, CRI 90).
  --
  -- /Note/ - Takes precedence over other F illuminants
  | F8
  | F9
  | F10
  | F11
  -- ^ Represents a narrow tri-band fluorescent of 4000° Kelvin color temperature, CRI
  -- 83.
  --
  -- /Note/ - Takes precedence over other F illuminants
  | F12
  deriving (Eq, Show)

-- | Academy Color Encoding System
data ACES =
  ACES
  deriving (Eq, Show)

instance Illuminant 'ACES where
  whitePoint = WhitePoint 0.32168 0.33767


-- Move into it's own module

-- -- | [DCI-P3](https://en.wikipedia.org/wiki/DCI-P3) is a color space from the American film industry
-- data DCI_P3 = DCI_P3

-- instance Illuminant 'DCI_P3 where
--   whitePoint = WhitePoint 0.314 0.351
