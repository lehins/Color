# color

| Language | Travis | Coveralls |Gitter.im |
|:--------:|:------:|:---------:|:--------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/color.svg) | [![Travis](https://img.shields.io/travis/lehins/color/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/lehins/color) | [![Coverage Status](https://coveralls.io/repos/github/lehins/color/badge.svg?branch=master)](https://coveralls.io/github/lehins/color?branch=master) | [![Join the chat at https://gitter.im/haskell-massiv/Lobby](https://badges.gitter.im/haskell-massiv/Lobby.svg)](https://gitter.im/haskell-massiv/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

|      Package       | Hackage | Nightly | LTS |
|:-------------------|:-------:|:-------:|:---:|
|  [`color`](https://github.com/lehins/color/tree/master/color)|                                       [![Hackage](https://img.shields.io/hackage/v/color.svg)](https://hackage.haskell.org/package/color)|                                                                                                        [![Nightly](https://www.stackage.org/package/color/badge/nightly)](https://www.stackage.org/nightly/package/color)|                                                                                         [![Nightly](https://www.stackage.org/package/color/badge/lts)](https://www.stackage.org/lts/package/color)|

A library for dealing with colors and pixels. Currently supported:

* Color models:

    * `Y`
    * `RGB`
    * `HSI`
    * `HSL`
    * `HSV`
    * `YCbCr`
    * `CMYK`

* Color spaces and arbitrary conversions between them:

    * `Y` - luminance
    * `XYZ`
    * `La*b*`
    * `RGB`:

        * `sRGB`
        * `AdobeRGB`
        * ITU: `Rec470`, `Rec601` and `Rec709`
        * Alternative representations:

            * `HSI`
            * `HSL`
            * `HSV`
            * `YCbCr`
            * `CMYK`

* Illuminants:

    * CIE1931 - 2 degree observer
    * CIE1964 - 10 degree observer
    * Some common alternatives

* Chromatic adptation:

    * VonKries adptation with transformations:

            * VonKries
            * Bradford (default)
            * Fairchild
            * CIECAM02

* Color Standards:

    * RAL
