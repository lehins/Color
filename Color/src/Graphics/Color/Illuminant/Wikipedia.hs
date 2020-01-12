{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.Color.Illuminant.Wikipedia
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Illuminant.Wikipedia
  ( Degree2(..)
  ) where

import Graphics.Color.Space.Internal

-- | @[x=0.44757, y=0.40745]@ - 2° Observer - Wikipedia
instance Illuminant 'A   where
  type Temperature 'A = 2856
  whitePoint = WhitePoint 0.44757 0.40745

-- | @[x=0.34842, y=0.35161]@ - 2° Observer - Wikipedia
instance Illuminant 'B   where
  type Temperature 'B = 4874
  whitePoint = WhitePoint 0.34842 0.35161

-- | @[x=0.31006, y=0.31616]@ - 2° Observer - Wikipedia
instance Illuminant 'C   where
  type Temperature 'C = 6774
  whitePoint = WhitePoint 0.31006 0.31616

-- | @[x=0.34567, y=0.35850]@ - 2° Observer - Wikipedia
instance Illuminant 'D50 where
  type Temperature 'D50 = 5003
  whitePoint = WhitePoint 0.34567 0.35850

-- | @[x=0.33242, y=0.34743]@ - 2° Observer - Wikipedia
instance Illuminant 'D55 where
  type Temperature 'D55 = 5503
  whitePoint = WhitePoint 0.33242 0.34743

-- | @[x=0.31271, y=0.32901]@ - 2° Observer - Wikipedia
instance Illuminant 'D65 where
  type Temperature 'D65 = 6504
  whitePoint = WhitePoint 0.31271 0.32902

-- | @[x=0.29902, y=0.31485]@ - 2° Observer - Wikipedia
instance Illuminant 'D75 where
  type Temperature 'D75 = 7504
  whitePoint = WhitePoint 0.29902 0.31485

-- | @[x=1/3, y=1/3]@ - 2° Observer - Wikipedia
instance Illuminant 'E   where
  type Temperature 'E = 5454
  whitePoint = WhitePoint (1 / 3) (1 / 3)


-- | @[x=0.31310, y=0.33727]@ - 2° Observer - Wikipedia
instance Illuminant 'F1  where
  type Temperature 'F1 = 6430
  whitePoint = WhitePoint 0.31310 0.33727

-- | @[x=0.37208, y=0.375129@ - 2° Observer - Wikipedia
instance Illuminant 'F2  where
  type Temperature 'F2 = 4230
  whitePoint = WhitePoint 0.37208 0.37529

-- | @[x=0.40910, y=0.39430]@ - 2° Observer - Wikipedia
instance Illuminant 'F3  where
  type Temperature 'F3 = 3450
  whitePoint = WhitePoint 0.40910 0.39430

-- | @[x=0.44018, y=0.40329]@ - 2° Observer - Wikipedia
instance Illuminant 'F4  where
  type Temperature 'F4 = 2940
  whitePoint = WhitePoint 0.44018 0.40329

-- | @[x=0.31379, y=0.34531]@ - 2° Observer - Wikipedia
instance Illuminant 'F5  where
  type Temperature 'F5 = 6350
  whitePoint = WhitePoint 0.31379 0.34531

-- | @[x=0.37790, y=0.38835]@ - 2° Observer - Wikipedia
instance Illuminant 'F6  where
  type Temperature 'F6 = 4150
  whitePoint = WhitePoint 0.37790 0.38835

-- | @[x=0.31292, y=0.32933]@ - 2° Observer - Wikipedia
instance Illuminant 'F7  where
  type Temperature 'F7 = 6500
  whitePoint = WhitePoint 0.31292 0.32933

-- | @[x=0.34588, y=0.35875]@ - 2° Observer - Wikipedia
instance Illuminant 'F8  where
  type Temperature 'F8 = 5000
  whitePoint = WhitePoint 0.34588 0.35875

-- | @[x=0.37417, y=0.37281]@ - 2° Observer - Wikipedia
instance Illuminant 'F9  where
  type Temperature 'F9 = 4150
  whitePoint = WhitePoint 0.37417 0.37281

-- | @[x=0.34609, y=0.35986]@ - 2° Observer - Wikipedia
instance Illuminant 'F10 where
  type Temperature 'F10 = 5000
  whitePoint = WhitePoint 0.34609 0.35986

-- | @[x=0.38052, y=0.37713]@ - 2° Observer - Wikipedia
instance Illuminant 'F11 where
  type Temperature 'F11 = 4000
  whitePoint = WhitePoint 0.38052 0.37713

-- | @[x=0.43695, y=0.40441]@ - 2° Observer - Wikipedia
instance Illuminant 'F12 where
  type Temperature 'F12 = 3000
  whitePoint = WhitePoint 0.43695 0.40441


-- | 2° observer [Standard
-- Illuminants](https://en.wikipedia.org/wiki/Standard_illuminant#White_point) listed on
-- Wikipedia. Despite the fact that they have slightly different chromaticity coordinates
-- than the actual CIE1931 standard papers, these are very commmonly used values. For
-- better interoperability it is better to use the actual
-- `Graphics.Color.Illuminant.CIE1931.CIE1931` illuminants.
--
-- @since 0.1.1
data Degree2
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
  | D65
  -- ^ Noon Daylight
  | D75
  -- ^ Overcast dayligh / North sky Daylight
  | E
  -- ^ Equal energy
  | F1
  -- ^ Daylight Fluorescent
  | F2
  -- ^ The fluorescent illuminant in most common use, represents cool white fluorescent
  -- (4100° Kelvin, CRI 60). Non-standard names include F, F02, Fcw, CWF, CWF2.
  --
  -- /Note/ - Takes precedence over other F illuminants
  | F3
  -- ^ White Fluorescent
  | F4
  -- ^ Warm White Fluorescent
  | F5
  -- ^ Daylight Fluorescent
  | F6
  -- ^ Lite White Fluorescent
  | F7
  -- ^ Represents a broadband fluorescent lamp, which approximates CIE illuminant `D65`
  -- (6500° Kelvin, CRI 90).
  --
  -- /Note/ - Takes precedence over other F illuminants
  | F8
  -- ^ `D50` simulator, Sylvania F40 Design 50 (F40DSGN50)
  | F9
  -- ^ Cool White Deluxe Fluorescent
  | F10
  -- ^ Philips TL85, Ultralume 50
  | F11
  -- ^ Philips TL84, SP41, Ultralume 40
  --
  -- Represents a narrow tri-band fluorescent of 4000° Kelvin color temperature, CRI 83.
  --
  -- /Note/ - Takes precedence over other F illuminants
  | F12
  -- ^ Philips TL83, Ultralume 30
  deriving (Eq, Show, Read, Enum, Bounded)
