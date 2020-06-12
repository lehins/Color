{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Illuminant.CIE1964
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Illuminant.CIE1964
  ( CIE1964(..)
  ) where

import Graphics.Color.Space.Internal (Illuminant(..), WhitePoint(..))
import qualified Graphics.Color.Illuminant.CIE1931 as I2


-- | @[x=0.45117, y=0.40594]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'A   where
  type Temperature 'A = 2856
  whitePoint = WhitePoint 0.45117 0.40594
  colorTemperature = I2.rectifyColorTemperature 2848 1.4350

-- | @[x=0.34980, y=0.35270]@ - CIE 1964 10° Observer -
-- /https://www.colour-science.org/
instance Illuminant 'B   where
  type Temperature 'B = 4874
  whitePoint = WhitePoint 0.34980 0.35270

-- | @[x=0.31039, y=0.31905]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'C   where
  type Temperature 'C = 6774
  whitePoint = WhitePoint 0.31039 0.31905

-- | @[x=0.34773, y=0.35952]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D50 where
  type Temperature 'D50 = 5003
  whitePoint = WhitePoint 0.34773 0.35952
  colorTemperature = I2.rectifyColorTemperature 5000 1.4380

-- | @[x=0.33412, y=0.34877]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D55 where
  type Temperature 'D55 = 5503
  whitePoint = WhitePoint 0.33412 0.34877
  colorTemperature = I2.rectifyColorTemperature 5500 1.4380

-- | @[x=0.32299, y=0.33928]@ - CIE 1964 10° Observer -
-- /https://www.colour-science.org (rounded to 5 decimal points)/
instance Illuminant 'D60 where
  type Temperature 'D60 = 6003
  whitePoint = WhitePoint 0.32299 0.33928
  colorTemperature = I2.rectifyColorTemperature 6000 1.4380

-- | @[x=0.31381, y=0.33098]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D65 where
  type Temperature 'D65 = 6504
  whitePoint = WhitePoint 0.31381 0.33098
  colorTemperature = I2.rectifyColorTemperature 6500 1.4380

-- | @[x=0.29968, y=0.31740]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D75 where
  type Temperature 'D75 = 7504
  whitePoint = WhitePoint 0.29968 0.31740
  colorTemperature = I2.rectifyColorTemperature 7500 1.4380

-- | @[x=1\/3, y=1\/3]@ - CIE 1964 10° Observer -
-- /https://www.colour-science.org/
instance Illuminant 'E   where
  type Temperature 'E = 5454
  whitePoint = WhitePoint (1 / 3) (1 / 3)

-- | @[x=0.31811, y=0.33559]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL1  where
  type Temperature 'FL1 = 6430
  whitePoint = WhitePoint 0.31811 0.33559

-- | @[x=0.37925, y=0.36733]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL2  where
  type Temperature 'FL2 = 4230
  whitePoint = WhitePoint 0.37925 0.36733

-- | @[x=0.41761, y=0.38324]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3  where
  type Temperature 'FL3 = 3450
  whitePoint = WhitePoint 0.41761 0.38324

-- | @[x=0.44920, y=0.39074]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL4  where
  type Temperature 'FL4 = 2940
  whitePoint = WhitePoint 0.44920 0.39074

-- | @[x=0.31975, y=0.34246]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL5  where
  type Temperature 'FL5 = 6350
  whitePoint = WhitePoint 0.31975 0.34246

-- | @[x=0.38660, y=0.37847]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL6  where
  type Temperature 'FL6 = 4150
  whitePoint = WhitePoint 0.38660 0.37847

-- | @[x=0.31569, y=0.32960]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL7  where
  type Temperature 'FL7 = 6500
  whitePoint = WhitePoint 0.31569 0.32960

-- | @[x=0.34902, y=0.35939]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL8  where
  type Temperature 'FL8 = 5000
  whitePoint = WhitePoint 0.34902 0.35939

-- | @[x=0.37829, y=0.37045]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL9  where
  type Temperature 'FL9 = 4150
  whitePoint = WhitePoint 0.37829 0.37045

-- | @[x=0.35090, y=0.35444]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL10 where
  type Temperature 'FL10 = 5000
  whitePoint = WhitePoint 0.35090 0.35444

-- | @[x=0.38541, y=0.37123]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL11 where
  type Temperature 'FL11 = 4000
  whitePoint = WhitePoint 0.38541 0.37123

-- | @[x=0.44256, y=0.39717]@ - CIE 1964 10° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL12 where
  type Temperature 'FL12 = 3000
  whitePoint = WhitePoint 0.44256 0.39717


-- | CIE 1964 10° observer illuminants
--
-- References:
--
-- * [CIE15: Technical Report: Colorimetry, 3rd edition](https://web.archive.org/web/20190510201823/https://www.cdvplus.cz/file/3-publikace-cie15-2004/)

data CIE1964
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
  | FL1
  -- ^ Daylight Fluorescent
  | FL2
  -- ^ The fluorescent illuminant in most common use, represents cool white fluorescent
  -- (4100° Kelvin, CRI 60). Non-standard names include F, F02, Fcw, CWF, CWF2.
  --
  -- /Note/ - Takes precedence over other F illuminants
  | FL3
  -- ^ White Fluorescent
  | FL4
  -- ^ Warm White Fluorescent
  | FL5
  -- ^ Daylight Fluorescent
  | FL6
  -- ^ Lite White Fluorescent
  | FL7
  -- ^ Represents a broadband fluorescent lamp, which approximates CIE illuminant `D65`
  -- (6500° Kelvin, CRI 90).
  --
  -- /Note/ - Takes precedence over other F illuminants
  | FL8
  -- ^ `D50` simulator, Sylvania F40 Design 50 (F40DSGN50)
  | FL9
  -- ^ Cool White Deluxe Fluorescent
  | FL10
  -- ^ Philips TL85, Ultralume 50
  | FL11
  -- ^ Philips TL84, SP41, Ultralume 40
  --
  -- Represents a narrow tri-band fluorescent of 4000° Kelvin color temperature, CRI 83.
  --
  -- /Note/ - Takes precedence over other F illuminants
  | FL12
  -- ^ Philips TL83, Ultralume 30
  deriving (Eq, Show, Read, Enum, Bounded)
