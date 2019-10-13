{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.ColorSpace.CIE1931.Internal
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

-- | @[x=0.44758, y=0.40745]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'A   where whitePoint = WhitePoint 0.44758 0.40745
-- | @[x=0.34842, y=0.35161]@ - CIE 1931 2° Observer -
-- /https://www.colour-science.org/
instance Illuminant 'B   where whitePoint = WhitePoint 0.34842 0.35161
-- | @[x=0.31006, y=0.31616]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'C   where whitePoint = WhitePoint 0.31006 0.31616
-- | @[x=0.34567, y=0.35851]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D50 where whitePoint = WhitePoint 0.34567 0.35851
-- | @[x=0.33243, y=0.34744]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D55 where whitePoint = WhitePoint 0.33243 0.34744
-- | @[x=0.32163, y=0.33774]@ - CIE 1931 2° Observer -
-- /https://www.colour-science.org (rounded to 5 decimal points)/
instance Illuminant 'D60 where whitePoint = WhitePoint 0.32163 0.33774
-- | @[x=0.31272, y=0.32903]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D65 where whitePoint = WhitePoint 0.31272 0.32903
-- | @[x=0.29903, y=0.31488]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'D75 where whitePoint = WhitePoint 0.29903 0.31488
-- | @[x=1/3, y=1/3]@ - CIE 1931 2° Observer -
-- /https://www.colour-science.org/
instance Illuminant 'E   where whitePoint = WhitePoint (1 / 3) (1 / 3)
-- | @[x=0.31310, y=0.33710]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL1  where whitePoint = WhitePoint 0.3131 0.3371
-- | @[x=0.37210, y=0.37510]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL2  where whitePoint = WhitePoint 0.3721 0.3751
-- | @[x=0.40910, y=0.39410]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3  where whitePoint = WhitePoint 0.4091 0.3941
-- | @[x=0.44020, y=0.40310]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL4  where whitePoint = WhitePoint 0.4402 0.4031
-- | @[x=0.31380, y=0.34520]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL5  where whitePoint = WhitePoint 0.3138 0.3452
-- | @[x=0.37790, y=0.38820]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL6  where whitePoint = WhitePoint 0.3779 0.3882
-- | @[x=0.31290, y=0.32920]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL7  where whitePoint = WhitePoint 0.3129 0.3292
-- | @[x=0.34580, y=0.35860]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL8  where whitePoint = WhitePoint 0.3458 0.3586
-- | @[x=0.37410, y=0.37270]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL9  where whitePoint = WhitePoint 0.3741 0.3727
-- | @[x=0.34580, y=0.35880]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL10 where whitePoint = WhitePoint 0.3458 0.3588
-- | @[x=0.38050, y=0.37690]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL11 where whitePoint = WhitePoint 0.3805 0.3769
-- | @[x=0.43700, y=0.40420]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL12 where whitePoint = WhitePoint 0.4370 0.4042


-- | CIE 1931 2° observer illuminants
--
-- References:
--
-- * [CIE15: Technical Report: Colorimetry, 3rd edition](https://web.archive.org/web/20190510201823/https://www.cdvplus.cz/file/3-publikace-cie15-2004/)
-- * [HunterLab: Equivalent White Light Sources and CIE Illuminants](https://web.archive.org/web/20050523033826/http://www.hunterlab.com:80/appnotes/an05_05.pdf)

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
