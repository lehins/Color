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
-- Module      : Graphics.ColorStandard.RAL
-- Copyright   : (c) Alexey Kuleshevich 2019-2020
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorStandard.RAL
  ( RAL(..)
  , Color(..)
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
  , curry'
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
  ) where

import Graphics.ColorSpace
import Graphics.ColorSpace.CIE1976.LAB
import Graphics.ColorAdaptation.VonKries

-- TODO: Find a home somewhere for this common illuminant
data D50

instance Illuminant D50 where
  type Temperature D50 = 5003
  whitePoint = WhitePoint 0.3457 0.3585

class Color std code where

  color :: ColorSpace cs i e => std code -> Pixel cs e


-- | Source: https://en.wikipedia.org/wiki/List_of_RAL_colors
data RAL (n :: k) = RAL


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
  , curry'
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
    :: ColorSpace cs i e => Pixel cs e

ral :: ColorSpace cs i e => Float -> Float -> Float -> Pixel cs e
ral l' a' b' = convert (PixelLAB l' a' b' :: Pixel (LAB D50) Float)

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
curry' = ral 55.557 6.493 58.255
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


instance Color RAL 1000 where color _ = greenBeige
instance Color RAL 1001 where color _ = beige
instance Color RAL 1002 where color _ = sandYellow
instance Color RAL 1003 where color _ = signalYellow
instance Color RAL 1004 where color _ = goldenYellow
instance Color RAL 1005 where color _ = honeyYellow
instance Color RAL 1006 where color _ = maizeYellow
instance Color RAL 1007 where color _ = daffodilYellow
instance Color RAL 1011 where color _ = brownBeige
instance Color RAL 1012 where color _ = lemonYellow
instance Color RAL 1013 where color _ = oysterWhite
instance Color RAL 1014 where color _ = ivory
instance Color RAL 1015 where color _ = lightIvory
instance Color RAL 1016 where color _ = sulfurYellow
instance Color RAL 1017 where color _ = saffronYellow
instance Color RAL 1018 where color _ = zincYellow
instance Color RAL 1019 where color _ = greyBeige
instance Color RAL 1020 where color _ = oliveYellow
instance Color RAL 1021 where color _ = rapeYellow
instance Color RAL 1023 where color _ = trafficYellow
instance Color RAL 1024 where color _ = ochreYellow
instance Color RAL 1026 where color _ = luminousYellow
instance Color RAL 1027 where color _ = curry'
instance Color RAL 1028 where color _ = melonYellow
instance Color RAL 1032 where color _ = broomYellow
instance Color RAL 1033 where color _ = dahliaYellow
instance Color RAL 1034 where color _ = pastelYellow
instance Color RAL 1035 where color _ = pearlBeige
instance Color RAL 1036 where color _ = pearlGold
instance Color RAL 1037 where color _ = sunYellow

instance Color RAL 2000 where color _ = yellowOrange
instance Color RAL 2001 where color _ = redOrange
instance Color RAL 2002 where color _ = vermilion
instance Color RAL 2003 where color _ = pastelOrange
instance Color RAL 2004 where color _ = pureOrange
instance Color RAL 2005 where color _ = luminousOrange
instance Color RAL 2007 where color _ = luminousBrightOrange
instance Color RAL 2008 where color _ = brightRedOrange
instance Color RAL 2009 where color _ = trafficOrange
instance Color RAL 2010 where color _ = signalOrange
instance Color RAL 2011 where color _ = deepOrange
instance Color RAL 2012 where color _ = salmonOrange
instance Color RAL 2013 where color _ = pearlOrange

instance Color RAL 3000 where color _ = flameRed
instance Color RAL 3001 where color _ = signalRed
instance Color RAL 3002 where color _ = carmineRed
instance Color RAL 3003 where color _ = rubyRed
instance Color RAL 3004 where color _ = purpleRed
instance Color RAL 3005 where color _ = wineRed
instance Color RAL 3007 where color _ = blackRed
instance Color RAL 3009 where color _ = oxideRed
instance Color RAL 3011 where color _ = brownRed
instance Color RAL 3012 where color _ = beigeRed
instance Color RAL 3013 where color _ = tomatoRed
instance Color RAL 3014 where color _ = antiquePink
instance Color RAL 3015 where color _ = lightPink
instance Color RAL 3016 where color _ = coralRed
instance Color RAL 3017 where color _ = rose
instance Color RAL 3018 where color _ = strawberryRed
instance Color RAL 3020 where color _ = trafficRed
instance Color RAL 3022 where color _ = salmonPink
instance Color RAL 3024 where color _ = luminousRed
instance Color RAL 3026 where color _ = luminousBrightRed
instance Color RAL 3027 where color _ = raspberryRed
instance Color RAL 3028 where color _ = pureRed
instance Color RAL 3031 where color _ = orientRed
instance Color RAL 3032 where color _ = pearlRubyRed
instance Color RAL 3033 where color _ = pearlPink

instance Color RAL 4001 where color _ = redLilac
instance Color RAL 4002 where color _ = redViolet
instance Color RAL 4003 where color _ = heatherViolet
instance Color RAL 4004 where color _ = claretViolet
instance Color RAL 4005 where color _ = blueLilac
instance Color RAL 4006 where color _ = trafficPurple
instance Color RAL 4007 where color _ = purpleViolet
instance Color RAL 4008 where color _ = signalViolet
instance Color RAL 4009 where color _ = pastelViolet
instance Color RAL 4010 where color _ = telemagenta
instance Color RAL 4011 where color _ = pearlViolet
instance Color RAL 4012 where color _ = pearlBlackberry

instance Color RAL 5000 where color _ = violetBlue
instance Color RAL 5001 where color _ = greenBlue
instance Color RAL 5002 where color _ = ultramarineBlue
instance Color RAL 5003 where color _ = sapphireBlue
instance Color RAL 5004 where color _ = blackBlue
instance Color RAL 5005 where color _ = signalBlue
instance Color RAL 5007 where color _ = brilliantBlue
instance Color RAL 5008 where color _ = greyBlue
instance Color RAL 5009 where color _ = azureBlue
instance Color RAL 5010 where color _ = gentianBlue
instance Color RAL 5011 where color _ = steelBlue
instance Color RAL 5012 where color _ = lightBlue
instance Color RAL 5013 where color _ = cobaltBlue
instance Color RAL 5014 where color _ = pigeonBlue
instance Color RAL 5015 where color _ = skyBlue
instance Color RAL 5017 where color _ = trafficBlue
instance Color RAL 5018 where color _ = turquoiseBlue
instance Color RAL 5019 where color _ = capriBlue
instance Color RAL 5020 where color _ = oceanBlue
instance Color RAL 5021 where color _ = waterBlue
instance Color RAL 5022 where color _ = nightBlue
instance Color RAL 5023 where color _ = distantBlue
instance Color RAL 5024 where color _ = pastelBlue
instance Color RAL 5025 where color _ = pearlGentianBlue
instance Color RAL 5026 where color _ = pearlNightBlue

instance Color RAL 6000 where color _ = patinaGreen
instance Color RAL 6001 where color _ = emeraldGreen
instance Color RAL 6002 where color _ = leafGreen
instance Color RAL 6003 where color _ = oliveGreen
instance Color RAL 6004 where color _ = blueGreen
instance Color RAL 6005 where color _ = mossGreen
instance Color RAL 6006 where color _ = greyOlive
instance Color RAL 6007 where color _ = bottleGreen
instance Color RAL 6008 where color _ = brownGreen
instance Color RAL 6009 where color _ = firGreen
instance Color RAL 6010 where color _ = grassGreen
instance Color RAL 6011 where color _ = resedaGreen
instance Color RAL 6012 where color _ = blackGreen
instance Color RAL 6013 where color _ = reedGreen
instance Color RAL 6014 where color _ = yellowOlive
instance Color RAL 6015 where color _ = blackOlive
instance Color RAL 6016 where color _ = turquoiseGreen
instance Color RAL 6017 where color _ = mayGreen
instance Color RAL 6018 where color _ = yellowGreen
instance Color RAL 6019 where color _ = pastelGreen
instance Color RAL 6020 where color _ = chromeGreen
instance Color RAL 6021 where color _ = paleGreen
instance Color RAL 6022 where color _ = oliveDrab
instance Color RAL 6024 where color _ = trafficGreen
instance Color RAL 6025 where color _ = fernGreen
instance Color RAL 6026 where color _ = opalGreen
instance Color RAL 6027 where color _ = lightGreen
instance Color RAL 6028 where color _ = pineGreen
instance Color RAL 6029 where color _ = mintGreen
instance Color RAL 6032 where color _ = signalGreen
instance Color RAL 6033 where color _ = mintTurquoise
instance Color RAL 6034 where color _ = pastelTurquoise
instance Color RAL 6035 where color _ = pearlGreen
instance Color RAL 6036 where color _ = pearlOpalGreen
instance Color RAL 6037 where color _ = pureGreen
instance Color RAL 6038 where color _ = luminousGreen

instance Color RAL 7000 where color _ = squirrelGrey
instance Color RAL 7001 where color _ = silverGrey
instance Color RAL 7002 where color _ = oliveGrey
instance Color RAL 7003 where color _ = mossGrey
instance Color RAL 7004 where color _ = signalGrey
instance Color RAL 7005 where color _ = mouseGrey
instance Color RAL 7006 where color _ = beigeGrey
instance Color RAL 7008 where color _ = khakiGrey
instance Color RAL 7009 where color _ = greenGrey
instance Color RAL 7010 where color _ = tarpaulinGrey
instance Color RAL 7011 where color _ = ironGrey
instance Color RAL 7012 where color _ = basaltGrey
instance Color RAL 7013 where color _ = brownGrey
instance Color RAL 7015 where color _ = slateGrey
instance Color RAL 7016 where color _ = anthraciteGrey
instance Color RAL 7021 where color _ = blackGrey
instance Color RAL 7022 where color _ = umbraGrey
instance Color RAL 7023 where color _ = concreteGrey
instance Color RAL 7024 where color _ = graphiteGrey
instance Color RAL 7026 where color _ = graniteGrey
instance Color RAL 7030 where color _ = stoneGrey
instance Color RAL 7031 where color _ = blueGrey
instance Color RAL 7032 where color _ = pebbleGrey
instance Color RAL 7033 where color _ = cementGrey
instance Color RAL 7034 where color _ = yellowGrey
instance Color RAL 7035 where color _ = lightGrey
instance Color RAL 7036 where color _ = platinumGrey
instance Color RAL 7037 where color _ = dustyGrey
instance Color RAL 7038 where color _ = agateGrey
instance Color RAL 7039 where color _ = quartzGrey
instance Color RAL 7040 where color _ = windowGrey
instance Color RAL 7042 where color _ = trafficGreyA
instance Color RAL 7043 where color _ = trafficGreyB
instance Color RAL 7044 where color _ = silkGrey
instance Color RAL 7045 where color _ = telegrey1
instance Color RAL 7046 where color _ = telegrey2
instance Color RAL 7047 where color _ = telegrey4
instance Color RAL 7048 where color _ = pearlMouseGrey

instance Color RAL 8000 where color _ = greenBrown
instance Color RAL 8001 where color _ = ochreBrown
instance Color RAL 8002 where color _ = signalBrown
instance Color RAL 8003 where color _ = clayBrown
instance Color RAL 8004 where color _ = copperBrown
instance Color RAL 8007 where color _ = fawnBrown
instance Color RAL 8008 where color _ = oliveBrown
instance Color RAL 8011 where color _ = nutBrown
instance Color RAL 8012 where color _ = redBrown
instance Color RAL 8014 where color _ = sepiaBrown
instance Color RAL 8015 where color _ = chestnutBrown
instance Color RAL 8016 where color _ = mahoganyBrown
instance Color RAL 8017 where color _ = chocolateBrown
instance Color RAL 8019 where color _ = greyBrown
instance Color RAL 8022 where color _ = blackBrown
instance Color RAL 8023 where color _ = orangeBrown
instance Color RAL 8024 where color _ = beigeBrown
instance Color RAL 8025 where color _ = paleBrown
instance Color RAL 8028 where color _ = terraBrown
instance Color RAL 8029 where color _ = pearlCopper

instance Color RAL 9001 where color _ = cream
instance Color RAL 9002 where color _ = greyWhite
instance Color RAL 9003 where color _ = signalWhite
instance Color RAL 9004 where color _ = signalBlack
instance Color RAL 9005 where color _ = jetBlack
instance Color RAL 9006 where color _ = whiteAluminium
instance Color RAL 9007 where color _ = greyAluminium
instance Color RAL 9010 where color _ = pureWhite
instance Color RAL 9011 where color _ = graphiteBlack
instance Color RAL 9016 where color _ = trafficWhite
instance Color RAL 9017 where color _ = trafficBlack
instance Color RAL 9018 where color _ = papyrusWhite
instance Color RAL 9022 where color _ = pearlLightGrey
instance Color RAL 9023 where color _ = pearlDarkGrey


instance Color RAL "Green beige" where color _ = color (RAL :: RAL 1000)
instance Color RAL "Beige" where color _ = color (RAL :: RAL 1001)
instance Color RAL "Sand yellow" where color _ = color (RAL :: RAL 1002)
instance Color RAL "Signal yellow" where color _ = color (RAL :: RAL 1003)
instance Color RAL "Golden yellow" where color _ = color (RAL :: RAL 1004)
instance Color RAL "Honey yellow" where color _ = color (RAL :: RAL 1005)
instance Color RAL "Maize yellow" where color _ = color (RAL :: RAL 1006)
instance Color RAL "Daffodil yellow" where color _ = color (RAL :: RAL 1007)
instance Color RAL "Brown beige" where color _ = color (RAL :: RAL 1011)
instance Color RAL "Lemon yellow" where color _ = color (RAL :: RAL 1012)
instance Color RAL "Oyster white" where color _ = color (RAL :: RAL 1013)
instance Color RAL "Ivory" where color _ = color (RAL :: RAL 1014)
instance Color RAL "Light ivory" where color _ = color (RAL :: RAL 1015)
instance Color RAL "Sulfur yellow" where color _ = color (RAL :: RAL 1016)
instance Color RAL "Saffron yellow" where color _ = color (RAL :: RAL 1017)
instance Color RAL "Zinc yellow" where color _ = color (RAL :: RAL 1018)
instance Color RAL "Grey beige" where color _ = color (RAL :: RAL 1019)
instance Color RAL "Olive yellow" where color _ = color (RAL :: RAL 1020)
instance Color RAL "Rape yellow" where color _ = color (RAL :: RAL 1021)
instance Color RAL "Traffic yellow" where color _ = color (RAL :: RAL 1023)
instance Color RAL "Ochre yellow" where color _ = color (RAL :: RAL 1024)
instance Color RAL "Luminous yellow" where color _ = color (RAL :: RAL 1026)
instance Color RAL "Curry" where color _ = color (RAL :: RAL 1027)
instance Color RAL "Melon yellow" where color _ = color (RAL :: RAL 1028)
instance Color RAL "Broom yellow" where color _ = color (RAL :: RAL 1032)
instance Color RAL "Dahlia yellow" where color _ = color (RAL :: RAL 1033)
instance Color RAL "Pastel yellow" where color _ = color (RAL :: RAL 1034)
instance Color RAL "Pearl beige" where color _ = color (RAL :: RAL 1035)
instance Color RAL "Pearl gold" where color _ = color (RAL :: RAL 1036)
instance Color RAL "Sun yellow" where color _ = color (RAL :: RAL 1037)
instance Color RAL "Yellow orange" where color _ = color (RAL :: RAL 2000)
instance Color RAL "Red orange" where color _ = color (RAL :: RAL 2001)
instance Color RAL "Vermilion" where color _ = color (RAL :: RAL 2002)
instance Color RAL "Pastel orange" where color _ = color (RAL :: RAL 2003)
instance Color RAL "Pure orange" where color _ = color (RAL :: RAL 2004)
instance Color RAL "Luminous orange" where color _ = color (RAL :: RAL 2005)
instance Color RAL "Luminous bright orange" where color _ = color (RAL :: RAL 2007)
instance Color RAL "Bright red orange" where color _ = color (RAL :: RAL 2008)
instance Color RAL "Traffic orange" where color _ = color (RAL :: RAL 2009)
instance Color RAL "Signal orange" where color _ = color (RAL :: RAL 2010)
instance Color RAL "Deep orange" where color _ = color (RAL :: RAL 2011)
instance Color RAL "Salmon orange" where color _ = color (RAL :: RAL 2012)
instance Color RAL "Pearl orange" where color _ = color (RAL :: RAL 2013)
instance Color RAL "Flame red" where color _ = color (RAL :: RAL 3000)
instance Color RAL "Signal red" where color _ = color (RAL :: RAL 3001)
instance Color RAL "Carmine red" where color _ = color (RAL :: RAL 3002)
instance Color RAL "Ruby red" where color _ = color (RAL :: RAL 3003)
instance Color RAL "Purple red" where color _ = color (RAL :: RAL 3004)
instance Color RAL "Wine red" where color _ = color (RAL :: RAL 3005)
instance Color RAL "Black red" where color _ = color (RAL :: RAL 3007)
instance Color RAL "Oxide red" where color _ = color (RAL :: RAL 3009)
instance Color RAL "Brown red" where color _ = color (RAL :: RAL 3011)
instance Color RAL "Beige red" where color _ = color (RAL :: RAL 3012)
instance Color RAL "Tomato red" where color _ = color (RAL :: RAL 3013)
instance Color RAL "Antique pink" where color _ = color (RAL :: RAL 3014)
instance Color RAL "Light pink" where color _ = color (RAL :: RAL 3015)
instance Color RAL "Coral red" where color _ = color (RAL :: RAL 3016)
instance Color RAL "Rose" where color _ = color (RAL :: RAL 3017)
instance Color RAL "Strawberry red" where color _ = color (RAL :: RAL 3018)
instance Color RAL "Traffic red" where color _ = color (RAL :: RAL 3020)
instance Color RAL "Salmon pink" where color _ = color (RAL :: RAL 3022)
instance Color RAL "Luminous red" where color _ = color (RAL :: RAL 3024)
instance Color RAL "Luminous bright red" where color _ = color (RAL :: RAL 3026)
instance Color RAL "Raspberry red" where color _ = color (RAL :: RAL 3027)
instance Color RAL "Pure red" where color _ = color (RAL :: RAL 3028)
instance Color RAL "Orient red" where color _ = color (RAL :: RAL 3031)
instance Color RAL "Pearl ruby red" where color _ = color (RAL :: RAL 3032)
instance Color RAL "Pearl pink" where color _ = color (RAL :: RAL 3033)
instance Color RAL "Red lilac" where color _ = color (RAL :: RAL 4001)
instance Color RAL "Red violet" where color _ = color (RAL :: RAL 4002)
instance Color RAL "Heather violet" where color _ = color (RAL :: RAL 4003)
instance Color RAL "Claret violet" where color _ = color (RAL :: RAL 4004)
instance Color RAL "Blue lilac" where color _ = color (RAL :: RAL 4005)
instance Color RAL "Traffic purple" where color _ = color (RAL :: RAL 4006)
instance Color RAL "Purple violet" where color _ = color (RAL :: RAL 4007)
instance Color RAL "Signal violet" where color _ = color (RAL :: RAL 4008)
instance Color RAL "Pastel violet" where color _ = color (RAL :: RAL 4009)
instance Color RAL "Telemagenta" where color _ = color (RAL :: RAL 4010)
instance Color RAL "Pearl violet" where color _ = color (RAL :: RAL 4011)
instance Color RAL "Pearl blackberry" where color _ = color (RAL :: RAL 4012)
instance Color RAL "Violet blue" where color _ = color (RAL :: RAL 5000)
instance Color RAL "Green blue" where color _ = color (RAL :: RAL 5001)
instance Color RAL "Ultramarine blue" where color _ = color (RAL :: RAL 5002)
instance Color RAL "Sapphire blue" where color _ = color (RAL :: RAL 5003)
instance Color RAL "Black blue" where color _ = color (RAL :: RAL 5004)
instance Color RAL "Signal blue" where color _ = color (RAL :: RAL 5005)
instance Color RAL "Brilliant blue" where color _ = color (RAL :: RAL 5007)
instance Color RAL "Grey blue" where color _ = color (RAL :: RAL 5008)
instance Color RAL "Azure blue" where color _ = color (RAL :: RAL 5009)
instance Color RAL "Gentian blue" where color _ = color (RAL :: RAL 5010)
instance Color RAL "Steel blue" where color _ = color (RAL :: RAL 5011)
instance Color RAL "Light blue" where color _ = color (RAL :: RAL 5012)
instance Color RAL "Cobalt blue" where color _ = color (RAL :: RAL 5013)
instance Color RAL "Pigeon blue" where color _ = color (RAL :: RAL 5014)
instance Color RAL "Sky blue" where color _ = color (RAL :: RAL 5015)
instance Color RAL "Traffic blue" where color _ = color (RAL :: RAL 5017)
instance Color RAL "Turquoise blue" where color _ = color (RAL :: RAL 5018)
instance Color RAL "Capri blue" where color _ = color (RAL :: RAL 5019)
instance Color RAL "Ocean blue" where color _ = color (RAL :: RAL 5020)
instance Color RAL "Water blue" where color _ = color (RAL :: RAL 5021)
instance Color RAL "Night blue" where color _ = color (RAL :: RAL 5022)
instance Color RAL "Distant blue" where color _ = color (RAL :: RAL 5023)
instance Color RAL "Pastel blue" where color _ = color (RAL :: RAL 5024)
instance Color RAL "Pearl Gentian blue" where color _ = color (RAL :: RAL 5025)
instance Color RAL "Pearl night blue" where color _ = color (RAL :: RAL 5026)
instance Color RAL "Patina green" where color _ = color (RAL :: RAL 6000)
instance Color RAL "Emerald green" where color _ = color (RAL :: RAL 6001)
instance Color RAL "Leaf green" where color _ = color (RAL :: RAL 6002)
instance Color RAL "Olive green" where color _ = color (RAL :: RAL 6003)
instance Color RAL "Blue green" where color _ = color (RAL :: RAL 6004)
instance Color RAL "Moss green" where color _ = color (RAL :: RAL 6005)
instance Color RAL "Grey olive" where color _ = color (RAL :: RAL 6006)
instance Color RAL "Bottle green" where color _ = color (RAL :: RAL 6007)
instance Color RAL "Brown green" where color _ = color (RAL :: RAL 6008)
instance Color RAL "Fir green" where color _ = color (RAL :: RAL 6009)
instance Color RAL "Grass green" where color _ = color (RAL :: RAL 6010)
instance Color RAL "Reseda green" where color _ = color (RAL :: RAL 6011)
instance Color RAL "Black green" where color _ = color (RAL :: RAL 6012)
instance Color RAL "Reed green" where color _ = color (RAL :: RAL 6013)
instance Color RAL "Yellow olive" where color _ = color (RAL :: RAL 6014)
instance Color RAL "Black olive" where color _ = color (RAL :: RAL 6015)
instance Color RAL "Turquoise green" where color _ = color (RAL :: RAL 6016)
instance Color RAL "May green" where color _ = color (RAL :: RAL 6017)
instance Color RAL "Yellow green" where color _ = color (RAL :: RAL 6018)
instance Color RAL "Pastel green" where color _ = color (RAL :: RAL 6019)
instance Color RAL "Chrome green" where color _ = color (RAL :: RAL 6020)
instance Color RAL "Pale green" where color _ = color (RAL :: RAL 6021)
instance Color RAL "Olive-drab/brown olive" where color _ = color (RAL :: RAL 6022)
instance Color RAL "Traffic green" where color _ = color (RAL :: RAL 6024)
instance Color RAL "Fern green" where color _ = color (RAL :: RAL 6025)
instance Color RAL "Opal green" where color _ = color (RAL :: RAL 6026)
instance Color RAL "Light green" where color _ = color (RAL :: RAL 6027)
instance Color RAL "Pine green" where color _ = color (RAL :: RAL 6028)
instance Color RAL "Mint green" where color _ = color (RAL :: RAL 6029)
instance Color RAL "Signal green" where color _ = color (RAL :: RAL 6032)
instance Color RAL "Mint turquoise" where color _ = color (RAL :: RAL 6033)
instance Color RAL "Pastel turquoise" where color _ = color (RAL :: RAL 6034)
instance Color RAL "Pearl green" where color _ = color (RAL :: RAL 6035)
instance Color RAL "Pearl opal green" where color _ = color (RAL :: RAL 6036)
instance Color RAL "Pure green" where color _ = color (RAL :: RAL 6037)
instance Color RAL "Luminous green" where color _ = color (RAL :: RAL 6038)
instance Color RAL "Squirrel grey" where color _ = color (RAL :: RAL 7000)
instance Color RAL "Silver grey" where color _ = color (RAL :: RAL 7001)
instance Color RAL "Olive grey" where color _ = color (RAL :: RAL 7002)
instance Color RAL "Moss grey" where color _ = color (RAL :: RAL 7003)
instance Color RAL "Signal grey" where color _ = color (RAL :: RAL 7004)
instance Color RAL "Mouse grey" where color _ = color (RAL :: RAL 7005)
instance Color RAL "Beige grey" where color _ = color (RAL :: RAL 7006)
instance Color RAL "Khaki grey" where color _ = color (RAL :: RAL 7008)
instance Color RAL "Green grey" where color _ = color (RAL :: RAL 7009)
instance Color RAL "Tarpaulin grey" where color _ = color (RAL :: RAL 7010)
instance Color RAL "Iron grey" where color _ = color (RAL :: RAL 7011)
instance Color RAL "Basalt grey" where color _ = color (RAL :: RAL 7012)
instance Color RAL "Brown grey" where color _ = color (RAL :: RAL 7013)
instance Color RAL "Slate grey" where color _ = color (RAL :: RAL 7015)
instance Color RAL "Anthracite grey" where color _ = color (RAL :: RAL 7016)
instance Color RAL "Black grey" where color _ = color (RAL :: RAL 7021)
instance Color RAL "Umbra grey" where color _ = color (RAL :: RAL 7022)
instance Color RAL "Concrete grey" where color _ = color (RAL :: RAL 7023)
instance Color RAL "Graphite grey" where color _ = color (RAL :: RAL 7024)
instance Color RAL "Granite grey" where color _ = color (RAL :: RAL 7026)
instance Color RAL "Stone grey" where color _ = color (RAL :: RAL 7030)
instance Color RAL "Blue grey" where color _ = color (RAL :: RAL 7031)
instance Color RAL "Pebble grey" where color _ = color (RAL :: RAL 7032)
instance Color RAL "Cement grey" where color _ = color (RAL :: RAL 7033)
instance Color RAL "Yellow grey" where color _ = color (RAL :: RAL 7034)
instance Color RAL "Light grey" where color _ = color (RAL :: RAL 7035)
instance Color RAL "Platinum grey" where color _ = color (RAL :: RAL 7036)
instance Color RAL "Dusty grey" where color _ = color (RAL :: RAL 7037)
instance Color RAL "Agate grey" where color _ = color (RAL :: RAL 7038)
instance Color RAL "Quartz grey" where color _ = color (RAL :: RAL 7039)
instance Color RAL "Window grey" where color _ = color (RAL :: RAL 7040)
instance Color RAL "Traffic grey A" where color _ = color (RAL :: RAL 7042)
instance Color RAL "Traffic grey B" where color _ = color (RAL :: RAL 7043)
instance Color RAL "Silk grey" where color _ = color (RAL :: RAL 7044)
instance Color RAL "Telegrey 1" where color _ = color (RAL :: RAL 7045)
instance Color RAL "Telegrey 2" where color _ = color (RAL :: RAL 7046)
instance Color RAL "Telegrey 4" where color _ = color (RAL :: RAL 7047)
instance Color RAL "Pearl mouse grey" where color _ = color (RAL :: RAL 7048)
instance Color RAL "Green brown" where color _ = color (RAL :: RAL 8000)
instance Color RAL "Ochre brown" where color _ = color (RAL :: RAL 8001)
instance Color RAL "Signal brown" where color _ = color (RAL :: RAL 8002)
instance Color RAL "Clay brown" where color _ = color (RAL :: RAL 8003)
instance Color RAL "Copper brown" where color _ = color (RAL :: RAL 8004)
instance Color RAL "Fawn brown" where color _ = color (RAL :: RAL 8007)
instance Color RAL "Olive brown" where color _ = color (RAL :: RAL 8008)
instance Color RAL "Nut brown" where color _ = color (RAL :: RAL 8011)
instance Color RAL "Red brown" where color _ = color (RAL :: RAL 8012)
instance Color RAL "Sepia brown" where color _ = color (RAL :: RAL 8014)
instance Color RAL "Chestnut brown" where color _ = color (RAL :: RAL 8015)
instance Color RAL "Mahogany brown" where color _ = color (RAL :: RAL 8016)
instance Color RAL "Chocolate brown" where color _ = color (RAL :: RAL 8017)
instance Color RAL "Grey brown" where color _ = color (RAL :: RAL 8019)
instance Color RAL "Black brown" where color _ = color (RAL :: RAL 8022)
instance Color RAL "Orange brown" where color _ = color (RAL :: RAL 8023)
instance Color RAL "Beige brown" where color _ = color (RAL :: RAL 8024)
instance Color RAL "Pale brown" where color _ = color (RAL :: RAL 8025)
instance Color RAL "Terra brown" where color _ = color (RAL :: RAL 8028)
instance Color RAL "Pearl copper" where color _ = color (RAL :: RAL 8029)
instance Color RAL "Cream" where color _ = color (RAL :: RAL 9001)
instance Color RAL "Grey white" where color _ = color (RAL :: RAL 9002)
instance Color RAL "Signal white" where color _ = color (RAL :: RAL 9003)
instance Color RAL "Signal black" where color _ = color (RAL :: RAL 9004)
instance Color RAL "Jet black" where color _ = color (RAL :: RAL 9005)
instance Color RAL "White aluminium" where color _ = color (RAL :: RAL 9006)
instance Color RAL "Grey aluminium" where color _ = color (RAL :: RAL 9007)
instance Color RAL "Pure white" where color _ = color (RAL :: RAL 9010)
instance Color RAL "Graphite black" where color _ = color (RAL :: RAL 9011)
instance Color RAL "Traffic white" where color _ = color (RAL :: RAL 9016)
instance Color RAL "Traffic black" where color _ = color (RAL :: RAL 9017)
instance Color RAL "Papyrus white" where color _ = color (RAL :: RAL 9018)
instance Color RAL "Pearl light grey" where color _ = color (RAL :: RAL 9022)
instance Color RAL "Pearl dark grey" where color _ = color (RAL :: RAL 9023)
