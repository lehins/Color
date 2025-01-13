# Color

A library for dealing with Colors and pixels. It implements arbitrary color space
conversion, chromatic adaptation and other color manipulations.

## Status

| Language | Github Actions | Coveralls |Gitter.im |
|:--------:|:--------------:|:---------:|:--------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/Color.svg) | [![Build Status](https://github.com/lehins/Color/workflows/Color-CI/badge.svg)](https://github.com/lehins/Color/actions) | [![Coverage Status](https://coveralls.io/repos/github/lehins/Color/badge.svg?branch=master)](https://coveralls.io/github/lehins/Color?branch=master) | [![Join the chat at https://gitter.im/haskell-massiv/Lobby](https://badges.gitter.im/haskell-massiv/Lobby.svg)](https://gitter.im/haskell-massiv/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

|      Package       | Hackage | Nightly | LTS |
|:-------------------|:-------:|:-------:|:---:|
|  [`Color`](https://github.com/lehins/Color/tree/master/Color)|                                       [![Hackage](https://img.shields.io/hackage/v/Color.svg)](https://hackage.haskell.org/package/Color)|                                                                                                        [![Nightly](https://www.stackage.org/package/Color/badge/nightly)](https://www.stackage.org/nightly/package/Color)|                                                                                         [![Nightly](https://www.stackage.org/package/Color/badge/lts)](https://www.stackage.org/lts/package/Color)|

## Description

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
  * `Y'` - luma
  * `CIE XYZ`
  * `CIE L*a*b*`
  * `DIN99`
  * `RGB`:

    * `sRGB` - both standardized and derived
    * `AdobeRGB` - both standardized and derived
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

      * `VonKries`
      * `Bradford` (default)
      * `Fairchild`
      * `CIECAM02`
      * `CMCCAT2000`

* Color Standards:

  * RAL
  * SVG

## Example

Here is a short example how this library can be used. Here we assume a GHCi session that
can be started like so:

```shell
$ stack ghci --package Color
```

### Perceived lightness

Let's say we need find the perceived lightness as described in [this StackOverflow
answer](https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color/56678483#56678483)
for an RGB triple `(128, 255, 65) :: (Word8, Word8, Word8)`.

Before we can attempt getting the lightness we need to do these two things:

1. Figure out what is the color space of the `RGB` triplet? In particular the `Illuminant`
   and the `Linearity` of the `RGB` color space.
2. Convert your `RGB` color to [`CIE
   L*a*b*`](https://en.wikipedia.org/wiki/CIELAB_color_space) and then we can get the `L*`
   out, which is the perceived lightness.

More often than not an RGB image will be encoded in non-linear sRGB color space with 8 bits
per channel, so we'll use that for this example:

```haskell
ghci> :set -XDataKinds
ghci> import Graphics.Color.Space
ghci> let rgb8 = ColorSRGB 128 255 65 :: Color (SRGB 'NonLinear) Word8
ghci> print rgb8
<SRGB 'NonLinear:(128,255, 65)>
```

Before we convert `sRGB` to `CIE L*a*b*` color space we need to increase the precision to
`Double`, because for now `Word8` is not supported by the `LAB` color space implementation:

```haskell
ghci> let rgb = toDouble <$> rgb8
ghci> print rgb
<SRGB 'NonLinear:( 0.5019607843137255, 1.0000000000000000, 0.2549019607843137)>
```

In order to convert to another color space without changing the `Illuminant` we can use
`convertColor` function. So here is how we convert to CIELAB and extract the perceived
lightness `L*`:

```haskell
ghci> let lab@(ColorLAB l _ _) = convertColor rgb :: Color (LAB D65) Double
ghci> lab
<LAB * D65:(90.0867507593648500,-65.7999116680496000,74.4643898323530600)>
ghci> l
90.08675075936485
```

### Color adaptation

When a change of `Illuminant` is also needed during color space conversion we can use
`convert` function

```haskell
ghci> import Graphics.Color.Adaptation (convert)
ghci> import qualified Graphics.Color.Illuminant.CIE1964 as CIE1964
ghci> let lab@(ColorLAB l _ _) = convert rgb :: Color (LAB 'CIE1964.D50) Double
ghci> lab
<LAB CIE1964 'D50:(90.2287735564601500,-59.3846969983265500,72.9304679742930800)>
```

## External resources

* `Color` is on a list of curated [Awesome Colour](https://awesome-colour.org/#haskell)
  resources.

* While working on this library the [colour-science.org](https://www.colour-science.org/)
  and their Python implementation of [colour](https://github.com/colour-science/colour)
  was used extensively as a reference.
