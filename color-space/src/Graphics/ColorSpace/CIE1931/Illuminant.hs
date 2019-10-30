{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE DataKinds #-}
-- |
-- Module      : Graphics.ColorSpace.CIE1931.Illuminant
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorSpace.CIE1931.Illuminant
  ( CIE1931(..)
  , ACES(..)
  , wavelengths
  ) where

import Graphics.ColorSpace.Algebra
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
instance Illuminant 'FL1  where whitePoint = WhitePoint 0.31310 0.33710

-- | @[x=0.37210, y=0.37510]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL2  where whitePoint = WhitePoint 0.37210 0.37510

-- | @[x=0.40910, y=0.39410]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3  where whitePoint = WhitePoint 0.40910 0.39410

-- | @[x=0.44020, y=0.40310]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL4  where whitePoint = WhitePoint 0.44020 0.40310

-- | @[x=0.31380, y=0.34520]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL5  where whitePoint = WhitePoint 0.31380 0.34520

-- | @[x=0.37790, y=0.38820]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL6  where whitePoint = WhitePoint 0.37790 0.38820

-- | @[x=0.31290, y=0.32920]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL7  where whitePoint = WhitePoint 0.31290 0.32920

-- | @[x=0.34580, y=0.35860]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL8  where whitePoint = WhitePoint 0.34580 0.35860

-- | @[x=0.37410, y=0.37270]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL9  where whitePoint = WhitePoint 0.37410 0.37270

-- | @[x=0.34580, y=0.35880]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL10 where whitePoint = WhitePoint 0.34580 0.35880

-- | @[x=0.38050, y=0.37690]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL11 where whitePoint = WhitePoint 0.38050 0.37690

-- | @[x=0.43700, y=0.40420]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL12 where whitePoint = WhitePoint 0.43700 0.40420


-- | @[x=0.44070, y=0.40330]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_1 where whitePoint = WhitePoint 0.44070 0.40330

-- | @[x=0.38080, y=0.37340]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_2 where whitePoint = WhitePoint 0.38080 0.37340

-- | @[x=0.31530, y=0.34390]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_3 where whitePoint = WhitePoint 0.31530 0.34390

-- | @[x=0.44290, y=0.40430]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_4 where whitePoint = WhitePoint 0.44290 0.40430

-- | @[x=0.37490, y=0.36720]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_5 where whitePoint = WhitePoint 0.37490 0.36720

-- | @[x=0.34880, y=0.36000]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_6 where whitePoint = WhitePoint 0.34880 0.36000

-- | @[x=0.43840, y=0.40450]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_7 where whitePoint = WhitePoint 0.43840 0.40450

-- | @[x=0.38200, y=0.38320]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_8 where whitePoint = WhitePoint 0.38200 0.38320

-- | @[x=0.34990, y=0.35910]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_9 where whitePoint = WhitePoint 0.34990 0.35910

-- | @[x=0.34550, y=0.35600]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_10 where whitePoint = WhitePoint 0.34550 0.35600

-- | @[x=0.32450, y=0.34340]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_11 where whitePoint = WhitePoint 0.32450 0.34340

-- | @[x=0.43770, y=0.40370]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_12 where whitePoint = WhitePoint 0.43770 0.40370

-- | @[x=0.38300, y=0.37240]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_13 where whitePoint = WhitePoint 0.38300 0.37240

-- | @[x=0.34470, y=0.36090]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_14 where whitePoint = WhitePoint 0.34470 0.36090

-- | @[x=0.31270, y=0.32880]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'FL3_15 where whitePoint = WhitePoint 0.31270 0.32880


-- | @[x=0.53300, y=0.41500]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'HP1  where whitePoint = WhitePoint 0.53300 0.41500

-- | @[x=0.47780, y=0.41580]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'HP2  where whitePoint = WhitePoint 0.47780 0.41580

-- | @[x=0.43020, y=0.40750]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'HP3  where whitePoint = WhitePoint 0.43020 0.40750

-- | @[x=0.38120, y=0.37970]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'HP4  where whitePoint = WhitePoint 0.38120 0.37970

-- | @[x=0.37760, y=0.37130]@ - CIE 1931 2° Observer -
-- /CIE15: Technical Report: Colorimetry, 3rd edition/
instance Illuminant 'HP5  where whitePoint = WhitePoint 0.37760 0.37130


-- | CIE 1931 2° observer illuminants
--
-- References:
--
-- * [CIE15: Technical Report: Colorimetry, 3rd edition](https://web.archive.org/web/20190510201823/https://www.cdvplus.cz/file/3-publikace-cie15-2004/)
-- * [HunterLab: Equivalent White Light Sources and CIE Illuminants](https://web.archive.org/web/20050523033826/http://www.hunterlab.com:80/appnotes/an05_05.pdf)

data CIE1931
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
  | FL3_1
  -- ^ Standard halophosphate lamp (New set of fluorescent lamps)
  | FL3_2
  -- ^ Standard halophosphate lamp (New set of fluorescent lamps)
  | FL3_3
  -- ^ Standard halophosphate lamp (New set of fluorescent lamps)
  | FL3_4
  -- ^ Deluxe type lamp (New set of fluorescent lamps)
  | FL3_5
  -- ^ Deluxe type lamp (New set of fluorescent lamps)
  | FL3_6
  -- ^ Deluxe type lamp (New set of fluorescent lamps)
  | FL3_7
  -- ^ Three band fluorescent lamp (New set of fluorescent lamps)
  | FL3_8
  -- ^ Three band fluorescent lamp (New set of fluorescent lamps)
  | FL3_9
  -- ^ Three band fluorescent lamp (New set of fluorescent lamps)
  | FL3_10
  -- ^ Three band fluorescent lamp (New set of fluorescent lamps)
  | FL3_11
  -- ^ Three band fluorescent lamp (New set of fluorescent lamps)
  | FL3_12
  -- ^ Multi-band fluorescent lamp (New set of fluorescent lamps)
  | FL3_13
  -- ^ Multi-band fluorescent lamp (New set of fluorescent lamps)
  | FL3_14
  -- ^ Multi-band fluorescent lamp (New set of fluorescent lamps)
  | FL3_15
  -- ^ `D65` simulator lamp (New set of fluorescent lamps)
  | HP1
  -- ^ Standard high pressure sodium lamp
  | HP2
  -- ^ Colour enhanced high pressure sodium lamp
  | HP3
  -- ^ High pressure metal halide lamp
  | HP4
  -- ^ High pressure metal halide lamp
  | HP5
  -- ^ High pressure metal halide lamp
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



wavelengths :: [(Double, V3 Double)]
wavelengths =
  [ (300, V3   0.04   0.02  0.00)
  , (305, V3   3.02   2.26  1.00)
  , (310, V3   6.00   4.50  2.00)
  , (315, V3  17.80  13.45  3.00)
  , (320, V3  29.60  22.40  4.00)
  , (325, V3  42.45  32.20  6.25)
  , (330, V3  55.30  42.00  8.50)
  , (335, V3  56.30  41.30  8.15)
  , (340, V3  57.30  40.60  7.80)
  , (345, V3  59.55  41.10  7.25)
  , (350, V3  61.80  41.60  6.70)
  , (355, V3  61.65  39.80  6.00)
  , (360, V3  61.50  38.00  5.30)
  , (365, V3  65.15  40.20  5.70)
  , (370, V3  68.80  42.40  6.10)
  , (375, V3  66.10  40.45  4.55)
  , (380, V3  63.40  38.50  3.00)
  , (385, V3  64.60  36.75  2.10)
  , (390, V3  65.80  35.00  1.20)
  , (395, V3  80.30  39.20  0.05)
  , (400, V3  94.80  43.40 -1.10)
  , (405, V3  99.80  44.85 -0.80)
  , (410, V3 104.80  46.30 -0.50)
  , (415, V3 105.35  45.10 -0.60)
  , (420, V3 105.90  43.90 -0.70)
  , (425, V3 101.35  40.50 -0.95)
  , (430, V3  96.80  37.10 -1.20)
  , (435, V3 105.35  36.90 -1.90)
  , (440, V3 113.90  36.70 -2.60)
  , (445, V3 119.75  36.30 -2.75)
  , (450, V3 125.60  35.90 -2.90)
  , (455, V3 125.55  34.25 -2.85)
  , (460, V3 125.50  32.60 -2.80)
  , (465, V3 123.40  30.25 -2.70)
  , (470, V3 121.30  27.90 -2.60)
  , (475, V3 121.30  26.10 -2.60)
  , (480, V3 121.30  24.30 -2.60)
  , (485, V3 117.40  22.20 -2.20)
  , (490, V3 113.50  20.10 -1.80)
  , (495, V3 113.30  18.15 -1.65)
  , (500, V3 113.10  16.20 -1.50)
  , (505, V3 111.95  14.70 -1.40)
  , (510, V3 110.80  13.20 -1.30)
  , (515, V3 108.65  10.90 -1.25)
  , (520, V3 106.50   8.60 -1.20)
  , (525, V3 107.65   7.35 -1.10)
  , (530, V3 108.80   6.10 -1.00)
  , (535, V3 107.05   5.15 -0.75)
  , (540, V3 105.30   4.20 -0.50)
  , (545, V3 104.85   3.05 -0.40)
  , (550, V3 104.40   1.90 -0.30)
  , (555, V3 102.20   0.95 -0.15)
  , (560, V3 100.00   0.00  0.00)
  , (565, V3  98.00  -0.80  0.10)
  , (570, V3  96.00  -1.60  0.20)
  , (575, V3  95.55  -2.55  0.35)
  , (580, V3  95.10  -3.50  0.50)
  , (585, V3  92.10  -3.50  1.30)
  , (590, V3  89.10  -3.50  2.10)
  , (595, V3  89.80  -4.65  2.65)
  , (600, V3  90.50  -5.80  3.20)
  , (605, V3  90.40  -6.50  3.65)
  , (610, V3  90.30  -7.20  4.10)
  , (615, V3  89.35  -7.90  4.40)
  , (620, V3  88.40  -8.60  4.70)
  , (625, V3  86.20  -9.05  4.90)
  , (630, V3  84.00  -9.50  5.10)
  , (635, V3  84.55 -10.20  5.90)
  , (640, V3  85.10 -10.90  6.70)
  , (645, V3  83.50 -10.80  7.00)
  , (650, V3  81.90 -10.70  7.30)
  , (655, V3  82.25 -11.35  7.95)
  , (660, V3  82.60 -12.00  8.60)
  , (665, V3  83.75 -13.00  9.20)
  , (670, V3  84.90 -14.00  9.80)
  , (675, V3  83.10 -13.80 10.00)
  , (680, V3  81.30 -13.60 10.20)
  , (685, V3  76.60 -12.80  9.25)
  , (690, V3  71.90 -12.00  8.30)
  , (695, V3  73.10 -12.65  8.95)
  , (700, V3  74.30 -13.30  9.60)
  , (705, V3  75.35 -13.10  9.05)
  , (710, V3  76.40 -12.90  8.50)
  , (715, V3  69.85 -11.75  7.75)
  , (720, V3  63.30 -10.60  7.00)
  , (725, V3  67.50 -11.10  7.30)
  , (730, V3  71.70 -11.60  7.60)
  , (735, V3  74.35 -11.90  7.80)
  , (740, V3  77.00 -12.20  8.00)
  , (745, V3  71.10 -11.20  7.35)
  , (750, V3  65.20 -10.20  6.70)
  , (755, V3  56.45  -9.00  5.95)
  , (760, V3  47.70  -7.80  5.20)
  , (765, V3  58.15  -9.50  6.30)
  , (770, V3  68.60 -11.20  7.40)
  , (775, V3  66.80 -10.80  7.10)
  , (780, V3  65.00 -10.40  6.80)
  , (785, V3  65.50 -10.50  6.90)
  , (790, V3  66.00 -10.60  7.00)
  , (795, V3  63.50 -10.15  6.70)
  , (800, V3  61.00  -9.70  6.40)
  , (805, V3  57.15  -9.00  5.95)
  , (810, V3  53.30  -8.30  5.50)
  , (815, V3  56.10  -8.80  5.80)
  , (820, V3  58.90  -9.30  6.10)
  , (825, V3  60.40  -9.55  6.30)
  , (830, V3  61.90  -9.80  6.50)
  ]
