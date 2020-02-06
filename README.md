# Color

| Language | Travis | Coveralls |Gitter.im |
|:--------:|:------:|:---------:|:--------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/Color.svg) | [![Travis](https://img.shields.io/travis/lehins/Color/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/lehins/Color) | [![Coverage Status](https://coveralls.io/repos/github/lehins/Color/badge.svg?branch=master)](https://coveralls.io/github/lehins/Color?branch=master) | [![Join the chat at https://gitter.im/haskell-massiv/Lobby](https://badges.gitter.im/haskell-massiv/Lobby.svg)](https://gitter.im/haskell-massiv/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

|      Package       | Hackage | Nightly | LTS |
|:-------------------|:-------:|:-------:|:---:|
|  [`Color`](https://github.com/lehins/Color/tree/master/Color)|                                       [![Hackage](https://img.shields.io/hackage/v/Color.svg)](https://hackage.haskell.org/package/Color)|                                                                                                        [![Nightly](https://www.stackage.org/package/Color/badge/nightly)](https://www.stackage.org/nightly/package/Color)|                                                                                         [![Nightly](https://www.stackage.org/package/Color/badge/lts)](https://www.stackage.org/lts/package/Color)|

A library for dealing with Colors and pixels. It implements arbitrary color space
conversion, chromatic adaptation and other color manipulations.

There is a clear separation between color models, color spaces and alternative
representations of color spaces. All are distinct at the type level. The goal is to
prevent mixups of incompatible color types as well as utilize type information for
conversion between them.

Currently supported:

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
  * `CIE XYZ`
  * `CIE L*a*b*`
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

* Chromatic adaptation:

  * VonKries adaptation with transformations:

      * VonKries
      * Bradford (default)
      * Fairchild
      * CIECAM02

* Color Standards:

  * RAL


# External resources

* `Color` is on a list of curated [Awesome Colour](https://awesome-colour.org/#haskell)
  resources.

* While working on this library the [colour-science.org](https://www.colour-science.org/)
  and their Python implementation of [colour](https://github.com/colour-science/colour)
  was used extensively as a reference.
