{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Graphics.Color.Standard.RAL
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.Color.Standard.RAL
  ( RAL(..)
  , D50
  , greenBeige
  , beige
  , sandYellow
  , signalYellow
  , goldenYellow
  , honeyYellow
  , maizeYellow
  , daffodilYellow
  , brownBeige
  , lemonYellow
  , oysterWhite
  , ivory
  , lightIvory
  , sulfurYellow
  , saffronYellow
  , zincYellow
  , greyBeige
  , oliveYellow
  , rapeYellow
  , trafficYellow
  , ochreYellow
  , luminousYellow
  , curryYellow
  , melonYellow
  , broomYellow
  , dahliaYellow
  , pastelYellow
  , pearlBeige
  , pearlGold
  , sunYellow
  , yellowOrange
  , redOrange
  , vermilion
  , pastelOrange
  , pureOrange
  , luminousOrange
  , luminousBrightOrange
  , brightRedOrange
  , trafficOrange
  , signalOrange
  , deepOrange
  , salmonOrange
  , pearlOrange
  , flameRed
  , signalRed
  , carmineRed
  , rubyRed
  , purpleRed
  , wineRed
  , blackRed
  , oxideRed
  , brownRed
  , beigeRed
  , tomatoRed
  , antiquePink
  , lightPink
  , coralRed
  , rose
  , strawberryRed
  , trafficRed
  , salmonPink
  , luminousRed
  , luminousBrightRed
  , raspberryRed
  , pureRed
  , orientRed
  , pearlRubyRed
  , pearlPink
  , redLilac
  , redViolet
  , heatherViolet
  , claretViolet
  , blueLilac
  , trafficPurple
  , purpleViolet
  , signalViolet
  , pastelViolet
  , telemagenta
  , pearlViolet
  , pearlBlackberry
  , violetBlue
  , greenBlue
  , ultramarineBlue
  , sapphireBlue
  , blackBlue
  , signalBlue
  , brilliantBlue
  , greyBlue
  , azureBlue
  , gentianBlue
  , steelBlue
  , lightBlue
  , cobaltBlue
  , pigeonBlue
  , skyBlue
  , trafficBlue
  , turquoiseBlue
  , capriBlue
  , oceanBlue
  , waterBlue
  , nightBlue
  , distantBlue
  , pastelBlue
  , pearlGentianBlue
  , pearlNightBlue
  , patinaGreen
  , emeraldGreen
  , leafGreen
  , oliveGreen
  , blueGreen
  , mossGreen
  , greyOlive
  , bottleGreen
  , brownGreen
  , firGreen
  , grassGreen
  , resedaGreen
  , blackGreen
  , reedGreen
  , yellowOlive
  , blackOlive
  , turquoiseGreen
  , mayGreen
  , yellowGreen
  , pastelGreen
  , chromeGreen
  , paleGreen
  , oliveDrab
  , trafficGreen
  , fernGreen
  , opalGreen
  , lightGreen
  , pineGreen
  , mintGreen
  , signalGreen
  , mintTurquoise
  , pastelTurquoise
  , pearlGreen
  , pearlOpalGreen
  , pureGreen
  , luminousGreen
  , squirrelGrey
  , silverGrey
  , oliveGrey
  , mossGrey
  , signalGrey
  , mouseGrey
  , beigeGrey
  , khakiGrey
  , greenGrey
  , tarpaulinGrey
  , ironGrey
  , basaltGrey
  , brownGrey
  , slateGrey
  , anthraciteGrey
  , blackGrey
  , umbraGrey
  , concreteGrey
  , graphiteGrey
  , graniteGrey
  , stoneGrey
  , blueGrey
  , pebbleGrey
  , cementGrey
  , yellowGrey
  , lightGrey
  , platinumGrey
  , dustyGrey
  , agateGrey
  , quartzGrey
  , windowGrey
  , trafficGreyA
  , trafficGreyB
  , silkGrey
  , telegrey1
  , telegrey2
  , telegrey4
  , pearlMouseGrey
  , greenBrown
  , ochreBrown
  , signalBrown
  , clayBrown
  , copperBrown
  , fawnBrown
  , oliveBrown
  , nutBrown
  , redBrown
  , sepiaBrown
  , chestnutBrown
  , mahoganyBrown
  , chocolateBrown
  , greyBrown
  , blackBrown
  , orangeBrown
  , beigeBrown
  , paleBrown
  , terraBrown
  , pearlCopper
  , cream
  , greyWhite
  , signalWhite
  , signalBlack
  , jetBlack
  , whiteAluminium
  , greyAluminium
  , pureWhite
  , graphiteBlack
  , trafficWhite
  , trafficBlack
  , papyrusWhite
  , pearlLightGrey
  , pearlDarkGrey
  , StandardColor(..)
  ) where

import Data.Typeable
import Graphics.Color.Space
import Graphics.Color.Space.CIE1976.LAB
import Graphics.Color.Adaptation.VonKries
import Graphics.Color.Space.RGB.SRGB

-- -- TODO: Find a home somewhere for this common illuminant
-- data D50

-- -- | @[x=0.44758, y=0.40745]@ - CIE 1931 2° Observer - Used for definition of RAL
-- instance Illuminant D50 where
--   type Temperature D50 = 5003
--   whitePoint = WhitePoint 0.3457 0.3585
--                         -- 0.345704, y = 0.358540 -- << PhotoRGB

class StandardColor std code where

  color :: ColorSpace cs i e => std code -> Color cs e


-- | Source: https://en.wikipedia.org/wiki/List_of_RAL_colors
data RAL (n :: k) = RAL

instance (Typeable n, Typeable k) => Show (RAL (n :: k)) where
  showsPrec n c
    | n == 0 = p . showsType c
    | otherwise = ('(' :) . p . showsType c . (')' :)
    where
      p = ("RAL " ++)


greenBeige
  , beige
  , sandYellow
  , signalYellow
  , goldenYellow
  , honeyYellow
  , maizeYellow
  , daffodilYellow
  , brownBeige
  , lemonYellow
  , oysterWhite
  , ivory
  , lightIvory
  , sulfurYellow
  , saffronYellow
  , zincYellow
  , greyBeige
  , oliveYellow
  , rapeYellow
  , trafficYellow
  , ochreYellow
  , luminousYellow
  , curryYellow
  , melonYellow
  , broomYellow
  , dahliaYellow
  , pastelYellow
  , pearlBeige
  , pearlGold
  , sunYellow
  , yellowOrange
  , redOrange
  , vermilion
  , pastelOrange
  , pureOrange
  , luminousOrange
  , luminousBrightOrange
  , brightRedOrange
  , trafficOrange
  , signalOrange
  , deepOrange
  , salmonOrange
  , pearlOrange
  , flameRed
  , signalRed
  , carmineRed
  , rubyRed
  , purpleRed
  , wineRed
  , blackRed
  , oxideRed
  , brownRed
  , beigeRed
  , tomatoRed
  , antiquePink
  , lightPink
  , coralRed
  , rose
  , strawberryRed
  , trafficRed
  , salmonPink
  , luminousRed
  , luminousBrightRed
  , raspberryRed
  , pureRed
  , orientRed
  , pearlRubyRed
  , pearlPink
  , redLilac
  , redViolet
  , heatherViolet
  , claretViolet
  , blueLilac
  , trafficPurple
  , purpleViolet
  , signalViolet
  , pastelViolet
  , telemagenta
  , pearlViolet
  , pearlBlackberry
  , violetBlue
  , greenBlue
  , ultramarineBlue
  , sapphireBlue
  , blackBlue
  , signalBlue
  , brilliantBlue
  , greyBlue
  , azureBlue
  , gentianBlue
  , steelBlue
  , lightBlue
  , cobaltBlue
  , pigeonBlue
  , skyBlue
  , trafficBlue
  , turquoiseBlue
  , capriBlue
  , oceanBlue
  , waterBlue
  , nightBlue
  , distantBlue
  , pastelBlue
  , pearlGentianBlue
  , pearlNightBlue
  , patinaGreen
  , emeraldGreen
  , leafGreen
  , oliveGreen
  , blueGreen
  , mossGreen
  , greyOlive
  , bottleGreen
  , brownGreen
  , firGreen
  , grassGreen
  , resedaGreen
  , blackGreen
  , reedGreen
  , yellowOlive
  , blackOlive
  , turquoiseGreen
  , mayGreen
  , yellowGreen
  , pastelGreen
  , chromeGreen
  , paleGreen
  , oliveDrab
  , trafficGreen
  , fernGreen
  , opalGreen
  , lightGreen
  , pineGreen
  , mintGreen
  , signalGreen
  , mintTurquoise
  , pastelTurquoise
  , pearlGreen
  , pearlOpalGreen
  , pureGreen
  , luminousGreen
  , squirrelGrey
  , silverGrey
  , oliveGrey
  , mossGrey
  , signalGrey
  , mouseGrey
  , beigeGrey
  , khakiGrey
  , greenGrey
  , tarpaulinGrey
  , ironGrey
  , basaltGrey
  , brownGrey
  , slateGrey
  , anthraciteGrey
  , blackGrey
  , umbraGrey
  , concreteGrey
  , graphiteGrey
  , graniteGrey
  , stoneGrey
  , blueGrey
  , pebbleGrey
  , cementGrey
  , yellowGrey
  , lightGrey
  , platinumGrey
  , dustyGrey
  , agateGrey
  , quartzGrey
  , windowGrey
  , trafficGreyA
  , trafficGreyB
  , silkGrey
  , telegrey1
  , telegrey2
  , telegrey4
  , pearlMouseGrey
  , greenBrown
  , ochreBrown
  , signalBrown
  , clayBrown
  , copperBrown
  , fawnBrown
  , oliveBrown
  , nutBrown
  , redBrown
  , sepiaBrown
  , chestnutBrown
  , mahoganyBrown
  , chocolateBrown
  , greyBrown
  , blackBrown
  , orangeBrown
  , beigeBrown
  , paleBrown
  , terraBrown
  , pearlCopper
  , cream
  , greyWhite
  , signalWhite
  , signalBlack
  , jetBlack
  , whiteAluminium
  , greyAluminium
  , pureWhite
  , graphiteBlack
  , trafficWhite
  , trafficBlack
  , papyrusWhite
  , pearlLightGrey
  , pearlDarkGrey
    :: ColorSpace cs i e => Color cs e

ral :: ColorSpace cs i e => Float -> Float -> Float -> Color cs e
ral l' a' b' = convert (ColorLAB l' a' b' :: Color (LAB D50) Float)

greenBeige = ral 76.022 -0.366 27.636
beige = ral 73.595 5.518 26.95
sandYellow = ral 71.934 7.362 36.744
signalYellow = ral 75.041 19.801 80.264
goldenYellow = ral 70.089 16.1 78.815
honeyYellow = ral 63.448 13.382 74.694
maizeYellow = ral 66.562 22.472 76.492
daffodilYellow = ral 66.5 27.308 80.402
brownBeige = ral 57.338 12.518 33.353
lemonYellow = ral 73.615 4.946 68.938
oysterWhite = ral 87.152 0.27 10.431
ivory = ral 80.411 2.763 24.175
lightIvory = ral 85.219 2.394 16.821
sulfurYellow = ral 87.29 -9.283 76.694
saffronYellow = ral 75.183 20.633 55.581
zincYellow = ral 83.353 3.462 75.829
greyBeige = ral 60.643 4.702 13.952
oliveYellow = ral 59.883 0.56 24.683
rapeYellow = ral 77.828 10.664 94.6
trafficYellow = ral 77.72 11.334 93.913
ochreYellow = ral 62.261 8.491 41.488
luminousYellow = ral 99.618 -17.229 116.966
curryYellow = ral 55.557 6.493 58.255
melonYellow = ral 73.671 31.654 95.458
broomYellow = ral 71.135 12.766 74.772
dahliaYellow = ral 71.74 27.78 71.677
pastelYellow = ral 70.94 22.91 49.962
pearlBeige = ral 55.258 1.866 11.775
pearlGold = ral 44.425 6.462 25.001
sunYellow = ral 68.889 27.62 74.504
yellowOrange = ral 58.201 37.297 68.683
redOrange = ral 45.824 44.129 47.554
vermilion = ral 44.441 52.797 43.768
pastelOrange = ral 64.235 44.142 61.832
pureOrange = ral 54.448 53.586 63.716
luminousOrange = ral 72.274 87.783 82.315
luminousBrightOrange = ral 88.914 52.782 97.982
brightRedOrange = ral 60.334 46.913 60.652
trafficOrange = ral 53.766 51.886 62.145
signalOrange = ral 53.28 42.985 49.867
deepOrange = ral 59.241 40.856 64.504
salmonOrange = ral 56.085 42.492 34.021
pearlOrange = ral 37.341 33.898 32.139
flameRed = ral 37.687 50.439 36.563
signalRed = ral 34.702 48.478 31.204
carmineRed = ral 34.458 48.832 31.869
rubyRed = ral 29.149 45.067 24.389
purpleRed = ral 23.903 35.433 16.085
wineRed = ral 19.699 30.019 12.525
blackRed = ral 16.437 14.617 5.268
oxideRed = ral 27.272 24.588 16.512
brownRed = ral 27.966 36.64 21.316
beigeRed = ral 61.394 22.436 22.979
tomatoRed = ral 35.45 43.402 30.523
antiquePink = ral 58.26 34.939 14.085
lightPink = ral 71.232 21.595 4.983
coralRed = ral 40.678 42.925 30.919
rose = ral 51.708 47.655 19.335
strawberryRed = ral 47.141 54.458 24.604
trafficRed = ral 40.511 59.32 47.967
salmonPink = ral 56.056 38.9 29.704
luminousRed = ral 61.253 83.212 65.195
luminousBrightRed = ral 63.701 87.132 70.01
raspberryRed = ral 38.686 53.68 20.868
pureRed = ral 45.358 60.958 44.231
orientRed = ral 39.32 47.086 24.708
pearlRubyRed = ral 25.034 36.705 17.139
pearlPink = ral 39.808 43.686 30.991
redLilac = ral 45.333 19.568 -14.009
redViolet = ral 36.8 35.858 8.343
heatherViolet = ral 54.247 44.659 -5.015
claretViolet = ral 23.577 34.298 0.517
blueLilac = ral 47.024 16.762 -25.226
trafficPurple = ral 36.964 46.56 -16.759
purpleViolet = ral 19.721 20.17 -7.946
signalViolet = ral 40.761 32.526 -20.561
pastelViolet = ral 58.22 11.056 -3.283
telemagenta = ral 46.538 54.36 -4.083
pearlViolet = ral 44.108 12.383 -18.407
pearlBlackberry = ral 45.891 4.098 -11.086
violetBlue = ral 32.585 -1.282 -21.686
greenBlue = ral 29.866 -9.761 -19.22
ultramarineBlue = ral 24.179 11.008 -42.748
sapphireBlue = ral 22.956 0.076 -20.213
blackBlue = ral 11.174 0.654 -7.457
signalBlue = ral 32.448 -6.68 -37.201
brilliantBlue = ral 42.929 -6.8 -23.609
greyBlue = ral 23.463 -3.174 -8.172
azureBlue = ral 37.421 -10.799 -19.905
gentianBlue = ral 30.95 -7.433 -32.954
steelBlue = ral 16.969 -1.198 -13.154
lightBlue = ral 53.135 -15.219 -32.486
cobaltBlue = ral 20.044 2.964 -23.406
pigeonBlue = ral 51.233 -2.817 -16.741
skyBlue = ral 48.193 -13.907 -36.485
trafficBlue = ral 34.824 -13.495 -36.357
turquoiseBlue = ral 52.285 -30.312 -9.335
capriBlue = ral 36.615 -11.411 -28.473
oceanBlue = ral 23.75 -20.682 -12.512
waterBlue = ral 43.325 -33.797 -10.417
nightBlue = ral 19.872 10.632 -28.485
distantBlue = ral 43.013 -3.442 -23.737
pastelBlue = ral 58.295 -10.309 -18.647
pearlGentianBlue = ral 41.102 -15.762 -16.748
pearlNightBlue = ral 19.227 1.728 -24.359
patinaGreen = ral 44.529 -23.651 5.316
emeraldGreen = ral 39.247 -28.094 23.342
leafGreen = ral 34.079 -24.697 23.999
oliveGreen = ral 34.335 -5.296 13.147
blueGreen = ral 25.484 -19.095 -4.31
mossGreen = ral 24.439 -20.569 4.665
greyOlive = ral 24.036 -1.163 7.105
bottleGreen = ral 19.792 -6.46 9.562
brownGreen = ral 21.675 -0.827 6.489
firGreen = ral 20.78 -8.546 5.213
grassGreen = ral 42.993 -22.872 26.093
resedaGreen = ral 49.931 -12.896 17.344
blackGreen = ral 24.554 -6.234 -0.13
reedGreen = ral 49.493 -2.158 16.371
yellowOlive = ral 27.639 0.59 7.89
blackOlive = ral 25.484 -1.59 4.15
turquoiseGreen = ral 38.439 -39.355 8.026
mayGreen = ral 49.029 -25.488 29.753
yellowGreen = ral 57.587 -35.153 42.334
pastelGreen = ral 80.385 -13.069 14.476
chromeGreen = ral 26.338 -8.365 10.002
paleGreen = ral 61.305 -11.717 16.056
oliveDrab = ral 21.685 1.103 8.802
trafficGreen = ral 47.925 -44.563 18.534
fernGreen = ral 43.932 -15.048 26.076
opalGreen = ral 34.35 -36.57 0.829
lightGreen = ral 71.56 -20.503 -3.86
pineGreen = ral 32.5 -17.091 6.069
mintGreen = ral 39.92 -47.213 19.273
signalGreen = ral 47.236 -37.788 16.942
mintTurquoise = ral 51.934 -22.925 -2.331
pastelTurquoise = ral 67.128 -16.7 -5.213
pearlGreen = ral 28.366 -27.124 18.409
pearlOpalGreen = ral 32.729 -25.854 0.881
pureGreen = ral 50.209 -53.031 41.386
luminousGreen = ral 62.308 -84.293 57.548
squirrelGrey = ral 55.673 -3.552 -4.905
silverGrey = ral 61.648 -2.346 -4.456
oliveGrey = ral 50.775 -0.044 12.64
mossGrey = ral 49.694 -1.244 7.793
signalGrey = ral 63.832 0.185 -0.439
mouseGrey = ral 46.213 -1.581 1.174
beigeGrey = ral 45.607 2.498 8.713
khakiGrey = ral 41.485 4.118 22.658
greenGrey = ral 40.2 -2.864 4.187
tarpaulinGrey = ral 38.442 -2.331 2.593
ironGrey = ral 37.44 -2.068 -3.02
basaltGrey = ral 39.159 -2.027 -1.452
brownGrey = ral 34.343 0.836 8.009
slateGrey = ral 35.155 -0.229 -3.737
anthraciteGrey = ral 25.926 -1.853 -3.407
blackGrey = ral 20.639 -0.806 -1.325
umbraGrey = ral 31.372 -0.001 3.652
concreteGrey = ral 53.265 -1.634 5.038
graphiteGrey = ral 30.964 -0.314 -3.685
graniteGrey = ral 27.434 -4.013 -3.107
stoneGrey = ral 59.041 -0.114 5.3
blueGrey = ral 43.16 -3.593 -4.523
pebbleGrey = ral 71.941 -0.909 8.469
cementGrey = ral 53.842 -3.737 7.189
yellowGrey = ral 56.857 0.031 14.835
lightGrey = ral 80.047 -1.207 1.387
platinumGrey = ral 61.233 1.346 1.022
dustyGrey = ral 51.431 -0.642 0.533
agateGrey = ral 71.73 -1.62 3.82
quartzGrey = ral 43.496 0.373 5.56
windowGrey = ral 64.7 -1.517 -2.498
trafficGreyA = ral 60.149 -1.676 0.035
trafficGreyB = ral 34.565 -1.532 0.596
silkGrey = ral 72.904 -0.082 5.939
telegrey1 = ral 60.348 -1.428 -1.836
telegrey2 = ral 55.438 -1.833 -3.188
telegrey4 = ral 80.668 -0.266 0.82
pearlMouseGrey = ral 51.991 0.679 5.105
greenBrown = ral 46.778 7.583 28.693
ochreBrown = ral 47.08 18.952 39.867
signalBrown = ral 37.391 16.881 16.707
clayBrown = ral 37.148 18.589 30.287
copperBrown = ral 38.964 26.631 27.038
fawnBrown = ral 33.824 15.336 23.6
oliveBrown = ral 35.151 13.225 28.492
nutBrown = ral 27.125 13.264 17.081
redBrown = ral 27.74 22.005 15.201
sepiaBrown = ral 24.029 7.289 12.866
chestnutBrown = ral 25.491 20.675 15.127
mahoganyBrown = ral 21.4 14.371 13.84
chocolateBrown = ral 21.544 8.972 7.368
greyBrown = ral 23.321 2.993 1.467
blackBrown = ral 8.139 1.932 -0.585
orangeBrown = ral 45.42 28.236 39.65
beigeBrown = ral 38.036 14.136 20.822
paleBrown = ral 39.85 9.63 14.49
terraBrown = ral 26.58 7.884 13.812
pearlCopper = ral 34.763 26.058 21.392
cream = ral 89.616 0.594 8.06
greyWhite = ral 85.07 -1.04 5.18
signalWhite = ral 93.223 -0.644 2.45
signalBlack = ral 17.464 0.429 -0.837
jetBlack = ral 4.041 0.091 -0.873
whiteAluminium = ral 66.321 -0.347 0.545
greyAluminium = ral 55.547 -0.061 2.142
pureWhite = ral 93.613 -0.425 6.008
graphiteBlack = ral 16.572 -0.383 -1.735
trafficWhite = ral 94.843 -0.921 3.28
trafficBlack = ral 16.521 0.709 -0.582
papyrusWhite = ral 81.343 -2.293 2.956
pearlLightGrey = ral 55.474 -0.381 1.165
pearlDarkGrey = ral 51.274 -0.7 -0.279


instance StandardColor RAL 1000 where color _ = greenBeige
instance StandardColor RAL 1001 where color _ = beige
instance StandardColor RAL 1002 where color _ = sandYellow
instance StandardColor RAL 1003 where color _ = signalYellow
instance StandardColor RAL 1004 where color _ = goldenYellow
instance StandardColor RAL 1005 where color _ = honeyYellow
instance StandardColor RAL 1006 where color _ = maizeYellow
instance StandardColor RAL 1007 where color _ = daffodilYellow
instance StandardColor RAL 1011 where color _ = brownBeige
instance StandardColor RAL 1012 where color _ = lemonYellow
instance StandardColor RAL 1013 where color _ = oysterWhite
instance StandardColor RAL 1014 where color _ = ivory
instance StandardColor RAL 1015 where color _ = lightIvory
instance StandardColor RAL 1016 where color _ = sulfurYellow
instance StandardColor RAL 1017 where color _ = saffronYellow
instance StandardColor RAL 1018 where color _ = zincYellow
instance StandardColor RAL 1019 where color _ = greyBeige
instance StandardColor RAL 1020 where color _ = oliveYellow
instance StandardColor RAL 1021 where color _ = rapeYellow
instance StandardColor RAL 1023 where color _ = trafficYellow
instance StandardColor RAL 1024 where color _ = ochreYellow
instance StandardColor RAL 1026 where color _ = luminousYellow
instance StandardColor RAL 1027 where color _ = curryYellow
instance StandardColor RAL 1028 where color _ = melonYellow
instance StandardColor RAL 1032 where color _ = broomYellow
instance StandardColor RAL 1033 where color _ = dahliaYellow
instance StandardColor RAL 1034 where color _ = pastelYellow
instance StandardColor RAL 1035 where color _ = pearlBeige
instance StandardColor RAL 1036 where color _ = pearlGold
instance StandardColor RAL 1037 where color _ = sunYellow

instance StandardColor RAL 2000 where color _ = yellowOrange
instance StandardColor RAL 2001 where color _ = redOrange
instance StandardColor RAL 2002 where color _ = vermilion
instance StandardColor RAL 2003 where color _ = pastelOrange
instance StandardColor RAL 2004 where color _ = pureOrange
instance StandardColor RAL 2005 where color _ = luminousOrange
instance StandardColor RAL 2007 where color _ = luminousBrightOrange
instance StandardColor RAL 2008 where color _ = brightRedOrange
instance StandardColor RAL 2009 where color _ = trafficOrange
instance StandardColor RAL 2010 where color _ = signalOrange
instance StandardColor RAL 2011 where color _ = deepOrange
instance StandardColor RAL 2012 where color _ = salmonOrange
instance StandardColor RAL 2013 where color _ = pearlOrange

instance StandardColor RAL 3000 where color _ = flameRed
instance StandardColor RAL 3001 where color _ = signalRed
instance StandardColor RAL 3002 where color _ = carmineRed
instance StandardColor RAL 3003 where color _ = rubyRed
instance StandardColor RAL 3004 where color _ = purpleRed
instance StandardColor RAL 3005 where color _ = wineRed
instance StandardColor RAL 3007 where color _ = blackRed
instance StandardColor RAL 3009 where color _ = oxideRed
instance StandardColor RAL 3011 where color _ = brownRed
instance StandardColor RAL 3012 where color _ = beigeRed
instance StandardColor RAL 3013 where color _ = tomatoRed
instance StandardColor RAL 3014 where color _ = antiquePink
instance StandardColor RAL 3015 where color _ = lightPink
instance StandardColor RAL 3016 where color _ = coralRed
instance StandardColor RAL 3017 where color _ = rose
instance StandardColor RAL 3018 where color _ = strawberryRed
instance StandardColor RAL 3020 where color _ = trafficRed
instance StandardColor RAL 3022 where color _ = salmonPink
instance StandardColor RAL 3024 where color _ = luminousRed
instance StandardColor RAL 3026 where color _ = luminousBrightRed
instance StandardColor RAL 3027 where color _ = raspberryRed
instance StandardColor RAL 3028 where color _ = pureRed
instance StandardColor RAL 3031 where color _ = orientRed
instance StandardColor RAL 3032 where color _ = pearlRubyRed
instance StandardColor RAL 3033 where color _ = pearlPink

instance StandardColor RAL 4001 where color _ = redLilac
instance StandardColor RAL 4002 where color _ = redViolet
instance StandardColor RAL 4003 where color _ = heatherViolet
instance StandardColor RAL 4004 where color _ = claretViolet
instance StandardColor RAL 4005 where color _ = blueLilac
instance StandardColor RAL 4006 where color _ = trafficPurple
instance StandardColor RAL 4007 where color _ = purpleViolet
instance StandardColor RAL 4008 where color _ = signalViolet
instance StandardColor RAL 4009 where color _ = pastelViolet
instance StandardColor RAL 4010 where color _ = telemagenta
instance StandardColor RAL 4011 where color _ = pearlViolet
instance StandardColor RAL 4012 where color _ = pearlBlackberry

instance StandardColor RAL 5000 where color _ = violetBlue
instance StandardColor RAL 5001 where color _ = greenBlue
instance StandardColor RAL 5002 where color _ = ultramarineBlue
instance StandardColor RAL 5003 where color _ = sapphireBlue
instance StandardColor RAL 5004 where color _ = blackBlue
instance StandardColor RAL 5005 where color _ = signalBlue
instance StandardColor RAL 5007 where color _ = brilliantBlue
instance StandardColor RAL 5008 where color _ = greyBlue
instance StandardColor RAL 5009 where color _ = azureBlue
instance StandardColor RAL 5010 where color _ = gentianBlue
instance StandardColor RAL 5011 where color _ = steelBlue
instance StandardColor RAL 5012 where color _ = lightBlue
instance StandardColor RAL 5013 where color _ = cobaltBlue
instance StandardColor RAL 5014 where color _ = pigeonBlue
instance StandardColor RAL 5015 where color _ = skyBlue
instance StandardColor RAL 5017 where color _ = trafficBlue
instance StandardColor RAL 5018 where color _ = turquoiseBlue
instance StandardColor RAL 5019 where color _ = capriBlue
instance StandardColor RAL 5020 where color _ = oceanBlue
instance StandardColor RAL 5021 where color _ = waterBlue
instance StandardColor RAL 5022 where color _ = nightBlue
instance StandardColor RAL 5023 where color _ = distantBlue
instance StandardColor RAL 5024 where color _ = pastelBlue
instance StandardColor RAL 5025 where color _ = pearlGentianBlue
instance StandardColor RAL 5026 where color _ = pearlNightBlue

instance StandardColor RAL 6000 where color _ = patinaGreen
instance StandardColor RAL 6001 where color _ = emeraldGreen
instance StandardColor RAL 6002 where color _ = leafGreen
instance StandardColor RAL 6003 where color _ = oliveGreen
instance StandardColor RAL 6004 where color _ = blueGreen
instance StandardColor RAL 6005 where color _ = mossGreen
instance StandardColor RAL 6006 where color _ = greyOlive
instance StandardColor RAL 6007 where color _ = bottleGreen
instance StandardColor RAL 6008 where color _ = brownGreen
instance StandardColor RAL 6009 where color _ = firGreen
instance StandardColor RAL 6010 where color _ = grassGreen
instance StandardColor RAL 6011 where color _ = resedaGreen
instance StandardColor RAL 6012 where color _ = blackGreen
instance StandardColor RAL 6013 where color _ = reedGreen
instance StandardColor RAL 6014 where color _ = yellowOlive
instance StandardColor RAL 6015 where color _ = blackOlive
instance StandardColor RAL 6016 where color _ = turquoiseGreen
instance StandardColor RAL 6017 where color _ = mayGreen
instance StandardColor RAL 6018 where color _ = yellowGreen
instance StandardColor RAL 6019 where color _ = pastelGreen
instance StandardColor RAL 6020 where color _ = chromeGreen
instance StandardColor RAL 6021 where color _ = paleGreen
instance StandardColor RAL 6022 where color _ = oliveDrab
instance StandardColor RAL 6024 where color _ = trafficGreen
instance StandardColor RAL 6025 where color _ = fernGreen
instance StandardColor RAL 6026 where color _ = opalGreen
instance StandardColor RAL 6027 where color _ = lightGreen
instance StandardColor RAL 6028 where color _ = pineGreen
instance StandardColor RAL 6029 where color _ = mintGreen
instance StandardColor RAL 6032 where color _ = signalGreen
instance StandardColor RAL 6033 where color _ = mintTurquoise
instance StandardColor RAL 6034 where color _ = pastelTurquoise
instance StandardColor RAL 6035 where color _ = pearlGreen
instance StandardColor RAL 6036 where color _ = pearlOpalGreen
instance StandardColor RAL 6037 where color _ = pureGreen
instance StandardColor RAL 6038 where color _ = luminousGreen

instance StandardColor RAL 7000 where color _ = squirrelGrey
instance StandardColor RAL 7001 where color _ = silverGrey
instance StandardColor RAL 7002 where color _ = oliveGrey
instance StandardColor RAL 7003 where color _ = mossGrey
instance StandardColor RAL 7004 where color _ = signalGrey
instance StandardColor RAL 7005 where color _ = mouseGrey
instance StandardColor RAL 7006 where color _ = beigeGrey
instance StandardColor RAL 7008 where color _ = khakiGrey
instance StandardColor RAL 7009 where color _ = greenGrey
instance StandardColor RAL 7010 where color _ = tarpaulinGrey
instance StandardColor RAL 7011 where color _ = ironGrey
instance StandardColor RAL 7012 where color _ = basaltGrey
instance StandardColor RAL 7013 where color _ = brownGrey
instance StandardColor RAL 7015 where color _ = slateGrey
instance StandardColor RAL 7016 where color _ = anthraciteGrey
instance StandardColor RAL 7021 where color _ = blackGrey
instance StandardColor RAL 7022 where color _ = umbraGrey
instance StandardColor RAL 7023 where color _ = concreteGrey
instance StandardColor RAL 7024 where color _ = graphiteGrey
instance StandardColor RAL 7026 where color _ = graniteGrey
instance StandardColor RAL 7030 where color _ = stoneGrey
instance StandardColor RAL 7031 where color _ = blueGrey
instance StandardColor RAL 7032 where color _ = pebbleGrey
instance StandardColor RAL 7033 where color _ = cementGrey
instance StandardColor RAL 7034 where color _ = yellowGrey
instance StandardColor RAL 7035 where color _ = lightGrey
instance StandardColor RAL 7036 where color _ = platinumGrey
instance StandardColor RAL 7037 where color _ = dustyGrey
instance StandardColor RAL 7038 where color _ = agateGrey
instance StandardColor RAL 7039 where color _ = quartzGrey
instance StandardColor RAL 7040 where color _ = windowGrey
instance StandardColor RAL 7042 where color _ = trafficGreyA
instance StandardColor RAL 7043 where color _ = trafficGreyB
instance StandardColor RAL 7044 where color _ = silkGrey
instance StandardColor RAL 7045 where color _ = telegrey1
instance StandardColor RAL 7046 where color _ = telegrey2
instance StandardColor RAL 7047 where color _ = telegrey4
instance StandardColor RAL 7048 where color _ = pearlMouseGrey

instance StandardColor RAL 8000 where color _ = greenBrown
instance StandardColor RAL 8001 where color _ = ochreBrown
instance StandardColor RAL 8002 where color _ = signalBrown
instance StandardColor RAL 8003 where color _ = clayBrown
instance StandardColor RAL 8004 where color _ = copperBrown
instance StandardColor RAL 8007 where color _ = fawnBrown
instance StandardColor RAL 8008 where color _ = oliveBrown
instance StandardColor RAL 8011 where color _ = nutBrown
instance StandardColor RAL 8012 where color _ = redBrown
instance StandardColor RAL 8014 where color _ = sepiaBrown
instance StandardColor RAL 8015 where color _ = chestnutBrown
instance StandardColor RAL 8016 where color _ = mahoganyBrown
instance StandardColor RAL 8017 where color _ = chocolateBrown
instance StandardColor RAL 8019 where color _ = greyBrown
instance StandardColor RAL 8022 where color _ = blackBrown
instance StandardColor RAL 8023 where color _ = orangeBrown
instance StandardColor RAL 8024 where color _ = beigeBrown
instance StandardColor RAL 8025 where color _ = paleBrown
instance StandardColor RAL 8028 where color _ = terraBrown
instance StandardColor RAL 8029 where color _ = pearlCopper

instance StandardColor RAL 9001 where color _ = cream
instance StandardColor RAL 9002 where color _ = greyWhite
instance StandardColor RAL 9003 where color _ = signalWhite
instance StandardColor RAL 9004 where color _ = signalBlack
instance StandardColor RAL 9005 where color _ = jetBlack
instance StandardColor RAL 9006 where color _ = whiteAluminium
instance StandardColor RAL 9007 where color _ = greyAluminium
instance StandardColor RAL 9010 where color _ = pureWhite
instance StandardColor RAL 9011 where color _ = graphiteBlack
instance StandardColor RAL 9016 where color _ = trafficWhite
instance StandardColor RAL 9017 where color _ = trafficBlack
instance StandardColor RAL 9018 where color _ = papyrusWhite
instance StandardColor RAL 9022 where color _ = pearlLightGrey
instance StandardColor RAL 9023 where color _ = pearlDarkGrey


instance StandardColor RAL "Green beige" where color _ = color (RAL :: RAL 1000)
instance StandardColor RAL "Beige" where color _ = color (RAL :: RAL 1001)
instance StandardColor RAL "Sand yellow" where color _ = color (RAL :: RAL 1002)
instance StandardColor RAL "Signal yellow" where color _ = color (RAL :: RAL 1003)
instance StandardColor RAL "Golden yellow" where color _ = color (RAL :: RAL 1004)
instance StandardColor RAL "Honey yellow" where color _ = color (RAL :: RAL 1005)
instance StandardColor RAL "Maize yellow" where color _ = color (RAL :: RAL 1006)
instance StandardColor RAL "Daffodil yellow" where color _ = color (RAL :: RAL 1007)
instance StandardColor RAL "Brown beige" where color _ = color (RAL :: RAL 1011)
instance StandardColor RAL "Lemon yellow" where color _ = color (RAL :: RAL 1012)
instance StandardColor RAL "Oyster white" where color _ = color (RAL :: RAL 1013)
instance StandardColor RAL "Ivory" where color _ = color (RAL :: RAL 1014)
instance StandardColor RAL "Light ivory" where color _ = color (RAL :: RAL 1015)
instance StandardColor RAL "Sulfur yellow" where color _ = color (RAL :: RAL 1016)
instance StandardColor RAL "Saffron yellow" where color _ = color (RAL :: RAL 1017)
instance StandardColor RAL "Zinc yellow" where color _ = color (RAL :: RAL 1018)
instance StandardColor RAL "Grey beige" where color _ = color (RAL :: RAL 1019)
instance StandardColor RAL "Olive yellow" where color _ = color (RAL :: RAL 1020)
instance StandardColor RAL "Rape yellow" where color _ = color (RAL :: RAL 1021)
instance StandardColor RAL "Traffic yellow" where color _ = color (RAL :: RAL 1023)
instance StandardColor RAL "Ochre yellow" where color _ = color (RAL :: RAL 1024)
instance StandardColor RAL "Luminous yellow" where color _ = color (RAL :: RAL 1026)
instance StandardColor RAL "Curry yellow" where color _ = color (RAL :: RAL 1027)
instance StandardColor RAL "Melon yellow" where color _ = color (RAL :: RAL 1028)
instance StandardColor RAL "Broom yellow" where color _ = color (RAL :: RAL 1032)
instance StandardColor RAL "Dahlia yellow" where color _ = color (RAL :: RAL 1033)
instance StandardColor RAL "Pastel yellow" where color _ = color (RAL :: RAL 1034)
instance StandardColor RAL "Pearl beige" where color _ = color (RAL :: RAL 1035)
instance StandardColor RAL "Pearl gold" where color _ = color (RAL :: RAL 1036)
instance StandardColor RAL "Sun yellow" where color _ = color (RAL :: RAL 1037)
instance StandardColor RAL "Yellow orange" where color _ = color (RAL :: RAL 2000)
instance StandardColor RAL "Red orange" where color _ = color (RAL :: RAL 2001)
instance StandardColor RAL "Vermilion" where color _ = color (RAL :: RAL 2002)
instance StandardColor RAL "Pastel orange" where color _ = color (RAL :: RAL 2003)
instance StandardColor RAL "Pure orange" where color _ = color (RAL :: RAL 2004)
instance StandardColor RAL "Luminous orange" where color _ = color (RAL :: RAL 2005)
instance StandardColor RAL "Luminous bright orange" where color _ = color (RAL :: RAL 2007)
instance StandardColor RAL "Bright red orange" where color _ = color (RAL :: RAL 2008)
instance StandardColor RAL "Traffic orange" where color _ = color (RAL :: RAL 2009)
instance StandardColor RAL "Signal orange" where color _ = color (RAL :: RAL 2010)
instance StandardColor RAL "Deep orange" where color _ = color (RAL :: RAL 2011)
instance StandardColor RAL "Salmon orange" where color _ = color (RAL :: RAL 2012)
instance StandardColor RAL "Pearl orange" where color _ = color (RAL :: RAL 2013)
instance StandardColor RAL "Flame red" where color _ = color (RAL :: RAL 3000)
instance StandardColor RAL "Signal red" where color _ = color (RAL :: RAL 3001)
instance StandardColor RAL "Carmine red" where color _ = color (RAL :: RAL 3002)
instance StandardColor RAL "Ruby red" where color _ = color (RAL :: RAL 3003)
instance StandardColor RAL "Purple red" where color _ = color (RAL :: RAL 3004)
instance StandardColor RAL "Wine red" where color _ = color (RAL :: RAL 3005)
instance StandardColor RAL "Black red" where color _ = color (RAL :: RAL 3007)
instance StandardColor RAL "Oxide red" where color _ = color (RAL :: RAL 3009)
instance StandardColor RAL "Brown red" where color _ = color (RAL :: RAL 3011)
instance StandardColor RAL "Beige red" where color _ = color (RAL :: RAL 3012)
instance StandardColor RAL "Tomato red" where color _ = color (RAL :: RAL 3013)
instance StandardColor RAL "Antique pink" where color _ = color (RAL :: RAL 3014)
instance StandardColor RAL "Light pink" where color _ = color (RAL :: RAL 3015)
instance StandardColor RAL "Coral red" where color _ = color (RAL :: RAL 3016)
instance StandardColor RAL "Rose" where color _ = color (RAL :: RAL 3017)
instance StandardColor RAL "Strawberry red" where color _ = color (RAL :: RAL 3018)
instance StandardColor RAL "Traffic red" where color _ = color (RAL :: RAL 3020)
instance StandardColor RAL "Salmon pink" where color _ = color (RAL :: RAL 3022)
instance StandardColor RAL "Luminous red" where color _ = color (RAL :: RAL 3024)
instance StandardColor RAL "Luminous bright red" where color _ = color (RAL :: RAL 3026)
instance StandardColor RAL "Raspberry red" where color _ = color (RAL :: RAL 3027)
instance StandardColor RAL "Pure red" where color _ = color (RAL :: RAL 3028)
instance StandardColor RAL "Orient red" where color _ = color (RAL :: RAL 3031)
instance StandardColor RAL "Pearl ruby red" where color _ = color (RAL :: RAL 3032)
instance StandardColor RAL "Pearl pink" where color _ = color (RAL :: RAL 3033)
instance StandardColor RAL "Red lilac" where color _ = color (RAL :: RAL 4001)
instance StandardColor RAL "Red violet" where color _ = color (RAL :: RAL 4002)
instance StandardColor RAL "Heather violet" where color _ = color (RAL :: RAL 4003)
instance StandardColor RAL "Claret violet" where color _ = color (RAL :: RAL 4004)
instance StandardColor RAL "Blue lilac" where color _ = color (RAL :: RAL 4005)
instance StandardColor RAL "Traffic purple" where color _ = color (RAL :: RAL 4006)
instance StandardColor RAL "Purple violet" where color _ = color (RAL :: RAL 4007)
instance StandardColor RAL "Signal violet" where color _ = color (RAL :: RAL 4008)
instance StandardColor RAL "Pastel violet" where color _ = color (RAL :: RAL 4009)
instance StandardColor RAL "Telemagenta" where color _ = color (RAL :: RAL 4010)
instance StandardColor RAL "Pearl violet" where color _ = color (RAL :: RAL 4011)
instance StandardColor RAL "Pearl blackberry" where color _ = color (RAL :: RAL 4012)
instance StandardColor RAL "Violet blue" where color _ = color (RAL :: RAL 5000)
instance StandardColor RAL "Green blue" where color _ = color (RAL :: RAL 5001)
instance StandardColor RAL "Ultramarine blue" where color _ = color (RAL :: RAL 5002)
instance StandardColor RAL "Sapphire blue" where color _ = color (RAL :: RAL 5003)
instance StandardColor RAL "Black blue" where color _ = color (RAL :: RAL 5004)
instance StandardColor RAL "Signal blue" where color _ = color (RAL :: RAL 5005)
instance StandardColor RAL "Brilliant blue" where color _ = color (RAL :: RAL 5007)
instance StandardColor RAL "Grey blue" where color _ = color (RAL :: RAL 5008)
instance StandardColor RAL "Azure blue" where color _ = color (RAL :: RAL 5009)
instance StandardColor RAL "Gentian blue" where color _ = color (RAL :: RAL 5010)
instance StandardColor RAL "Steel blue" where color _ = color (RAL :: RAL 5011)
instance StandardColor RAL "Light blue" where color _ = color (RAL :: RAL 5012)
instance StandardColor RAL "Cobalt blue" where color _ = color (RAL :: RAL 5013)
instance StandardColor RAL "Pigeon blue" where color _ = color (RAL :: RAL 5014)
instance StandardColor RAL "Sky blue" where color _ = color (RAL :: RAL 5015)
instance StandardColor RAL "Traffic blue" where color _ = color (RAL :: RAL 5017)
instance StandardColor RAL "Turquoise blue" where color _ = color (RAL :: RAL 5018)
instance StandardColor RAL "Capri blue" where color _ = color (RAL :: RAL 5019)
instance StandardColor RAL "Ocean blue" where color _ = color (RAL :: RAL 5020)
instance StandardColor RAL "Water blue" where color _ = color (RAL :: RAL 5021)
instance StandardColor RAL "Night blue" where color _ = color (RAL :: RAL 5022)
instance StandardColor RAL "Distant blue" where color _ = color (RAL :: RAL 5023)
instance StandardColor RAL "Pastel blue" where color _ = color (RAL :: RAL 5024)
instance StandardColor RAL "Pearl gentian blue" where color _ = color (RAL :: RAL 5025)
instance StandardColor RAL "Pearl night blue" where color _ = color (RAL :: RAL 5026)
instance StandardColor RAL "Patina green" where color _ = color (RAL :: RAL 6000)
instance StandardColor RAL "Emerald green" where color _ = color (RAL :: RAL 6001)
instance StandardColor RAL "Leaf green" where color _ = color (RAL :: RAL 6002)
instance StandardColor RAL "Olive green" where color _ = color (RAL :: RAL 6003)
instance StandardColor RAL "Blue green" where color _ = color (RAL :: RAL 6004)
instance StandardColor RAL "Moss green" where color _ = color (RAL :: RAL 6005)
instance StandardColor RAL "Grey olive" where color _ = color (RAL :: RAL 6006)
instance StandardColor RAL "Bottle green" where color _ = color (RAL :: RAL 6007)
instance StandardColor RAL "Brown green" where color _ = color (RAL :: RAL 6008)
instance StandardColor RAL "Fir green" where color _ = color (RAL :: RAL 6009)
instance StandardColor RAL "Grass green" where color _ = color (RAL :: RAL 6010)
instance StandardColor RAL "Reseda green" where color _ = color (RAL :: RAL 6011)
instance StandardColor RAL "Black green" where color _ = color (RAL :: RAL 6012)
instance StandardColor RAL "Reed green" where color _ = color (RAL :: RAL 6013)
instance StandardColor RAL "Yellow olive" where color _ = color (RAL :: RAL 6014)
instance StandardColor RAL "Black olive" where color _ = color (RAL :: RAL 6015)
instance StandardColor RAL "Turquoise green" where color _ = color (RAL :: RAL 6016)
instance StandardColor RAL "May green" where color _ = color (RAL :: RAL 6017)
instance StandardColor RAL "Yellow green" where color _ = color (RAL :: RAL 6018)
instance StandardColor RAL "Pastel green" where color _ = color (RAL :: RAL 6019)
instance StandardColor RAL "Chrome green" where color _ = color (RAL :: RAL 6020)
instance StandardColor RAL "Pale green" where color _ = color (RAL :: RAL 6021)
instance StandardColor RAL "Olive-drab" where color _ = color (RAL :: RAL 6022)
instance StandardColor RAL "Brown olive" where color _ = color (RAL :: RAL 6022)
instance StandardColor RAL "Traffic green" where color _ = color (RAL :: RAL 6024)
instance StandardColor RAL "Fern green" where color _ = color (RAL :: RAL 6025)
instance StandardColor RAL "Opal green" where color _ = color (RAL :: RAL 6026)
instance StandardColor RAL "Light green" where color _ = color (RAL :: RAL 6027)
instance StandardColor RAL "Pine green" where color _ = color (RAL :: RAL 6028)
instance StandardColor RAL "Mint green" where color _ = color (RAL :: RAL 6029)
instance StandardColor RAL "Signal green" where color _ = color (RAL :: RAL 6032)
instance StandardColor RAL "Mint turquoise" where color _ = color (RAL :: RAL 6033)
instance StandardColor RAL "Pastel turquoise" where color _ = color (RAL :: RAL 6034)
instance StandardColor RAL "Pearl green" where color _ = color (RAL :: RAL 6035)
instance StandardColor RAL "Pearl opal green" where color _ = color (RAL :: RAL 6036)
instance StandardColor RAL "Pure green" where color _ = color (RAL :: RAL 6037)
instance StandardColor RAL "Luminous green" where color _ = color (RAL :: RAL 6038)
instance StandardColor RAL "Squirrel grey" where color _ = color (RAL :: RAL 7000)
instance StandardColor RAL "Silver grey" where color _ = color (RAL :: RAL 7001)
instance StandardColor RAL "Olive grey" where color _ = color (RAL :: RAL 7002)
instance StandardColor RAL "Moss grey" where color _ = color (RAL :: RAL 7003)
instance StandardColor RAL "Signal grey" where color _ = color (RAL :: RAL 7004)
instance StandardColor RAL "Mouse grey" where color _ = color (RAL :: RAL 7005)
instance StandardColor RAL "Beige grey" where color _ = color (RAL :: RAL 7006)
instance StandardColor RAL "Khaki grey" where color _ = color (RAL :: RAL 7008)
instance StandardColor RAL "Green grey" where color _ = color (RAL :: RAL 7009)
instance StandardColor RAL "Tarpaulin grey" where color _ = color (RAL :: RAL 7010)
instance StandardColor RAL "Iron grey" where color _ = color (RAL :: RAL 7011)
instance StandardColor RAL "Basalt grey" where color _ = color (RAL :: RAL 7012)
instance StandardColor RAL "Brown grey" where color _ = color (RAL :: RAL 7013)
instance StandardColor RAL "NATO olive" where color _ = color (RAL :: RAL 7013)
instance StandardColor RAL "Slate grey" where color _ = color (RAL :: RAL 7015)
instance StandardColor RAL "Anthracite grey" where color _ = color (RAL :: RAL 7016)
instance StandardColor RAL "Black grey" where color _ = color (RAL :: RAL 7021)
instance StandardColor RAL "Umbra grey" where color _ = color (RAL :: RAL 7022)
instance StandardColor RAL "Concrete grey" where color _ = color (RAL :: RAL 7023)
instance StandardColor RAL "Graphite grey" where color _ = color (RAL :: RAL 7024)
instance StandardColor RAL "Granite grey" where color _ = color (RAL :: RAL 7026)
instance StandardColor RAL "Stone grey" where color _ = color (RAL :: RAL 7030)
instance StandardColor RAL "Blue grey" where color _ = color (RAL :: RAL 7031)
instance StandardColor RAL "Pebble grey" where color _ = color (RAL :: RAL 7032)
instance StandardColor RAL "Cement grey" where color _ = color (RAL :: RAL 7033)
instance StandardColor RAL "Yellow grey" where color _ = color (RAL :: RAL 7034)
instance StandardColor RAL "Light grey" where color _ = color (RAL :: RAL 7035)
instance StandardColor RAL "Platinum grey" where color _ = color (RAL :: RAL 7036)
instance StandardColor RAL "Dusty grey" where color _ = color (RAL :: RAL 7037)
instance StandardColor RAL "Agate grey" where color _ = color (RAL :: RAL 7038)
instance StandardColor RAL "Quartz grey" where color _ = color (RAL :: RAL 7039)
instance StandardColor RAL "Window grey" where color _ = color (RAL :: RAL 7040)
instance StandardColor RAL "Traffic grey A" where color _ = color (RAL :: RAL 7042)
instance StandardColor RAL "Traffic grey B" where color _ = color (RAL :: RAL 7043)
instance StandardColor RAL "Silk grey" where color _ = color (RAL :: RAL 7044)
instance StandardColor RAL "Telegrey 1" where color _ = color (RAL :: RAL 7045)
instance StandardColor RAL "Telegrey 2" where color _ = color (RAL :: RAL 7046)
instance StandardColor RAL "Telegrey 4" where color _ = color (RAL :: RAL 7047)
instance StandardColor RAL "Pearl mouse grey" where color _ = color (RAL :: RAL 7048)
instance StandardColor RAL "Green brown" where color _ = color (RAL :: RAL 8000)
instance StandardColor RAL "Ochre brown" where color _ = color (RAL :: RAL 8001)
instance StandardColor RAL "Signal brown" where color _ = color (RAL :: RAL 8002)
instance StandardColor RAL "Clay brown" where color _ = color (RAL :: RAL 8003)
instance StandardColor RAL "Copper brown" where color _ = color (RAL :: RAL 8004)
instance StandardColor RAL "Fawn brown" where color _ = color (RAL :: RAL 8007)
instance StandardColor RAL "Olive brown" where color _ = color (RAL :: RAL 8008)
instance StandardColor RAL "Nut brown" where color _ = color (RAL :: RAL 8011)
instance StandardColor RAL "Red brown" where color _ = color (RAL :: RAL 8012)
instance StandardColor RAL "Sepia brown" where color _ = color (RAL :: RAL 8014)
instance StandardColor RAL "Chestnut brown" where color _ = color (RAL :: RAL 8015)
instance StandardColor RAL "Mahogany brown" where color _ = color (RAL :: RAL 8016)
instance StandardColor RAL "Chocolate brown" where color _ = color (RAL :: RAL 8017)
instance StandardColor RAL "Grey brown" where color _ = color (RAL :: RAL 8019)
instance StandardColor RAL "Black brown" where color _ = color (RAL :: RAL 8022)
instance StandardColor RAL "Orange brown" where color _ = color (RAL :: RAL 8023)
instance StandardColor RAL "Beige brown" where color _ = color (RAL :: RAL 8024)
instance StandardColor RAL "Pale brown" where color _ = color (RAL :: RAL 8025)
instance StandardColor RAL "Terra brown" where color _ = color (RAL :: RAL 8028)
instance StandardColor RAL "Pearl copper" where color _ = color (RAL :: RAL 8029)
instance StandardColor RAL "Cream" where color _ = color (RAL :: RAL 9001)
instance StandardColor RAL "Grey white" where color _ = color (RAL :: RAL 9002)
instance StandardColor RAL "Signal white" where color _ = color (RAL :: RAL 9003)
instance StandardColor RAL "Signal black" where color _ = color (RAL :: RAL 9004)
instance StandardColor RAL "Jet black" where color _ = color (RAL :: RAL 9005)
instance StandardColor RAL "White aluminium" where color _ = color (RAL :: RAL 9006)
instance StandardColor RAL "Grey aluminium" where color _ = color (RAL :: RAL 9007)
instance StandardColor RAL "Pure white" where color _ = color (RAL :: RAL 9010)
instance StandardColor RAL "Graphite black" where color _ = color (RAL :: RAL 9011)
instance StandardColor RAL "Traffic white" where color _ = color (RAL :: RAL 9016)
instance StandardColor RAL "Traffic black" where color _ = color (RAL :: RAL 9017)
instance StandardColor RAL "Papyrus white" where color _ = color (RAL :: RAL 9018)
instance StandardColor RAL "Pearl light grey" where color _ = color (RAL :: RAL 9022)
instance StandardColor RAL "Pearl dark grey" where color _ = color (RAL :: RAL 9023)
