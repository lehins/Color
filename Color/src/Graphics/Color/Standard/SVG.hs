{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Graphics.Color.Standard.SVG
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
-- Source: https://www.w3.org/TR/SVG11/types.html#ColorKeywords
module Graphics.Color.Standard.SVG where
-- This file is generated with files/mkStdSVGModule

import Prelude hiding (tan)
import Graphics.Color.Space.RGB
import GHC.TypeLits
import Graphics.Color.Adaptation.VonKries
import Graphics.Color.Standard.Internal

-- | Get a color value by specifying its SVG standard name at the type level:
--
-- >>> import Graphics.Color.Standard
-- >>> import Graphics.Color.Space.CIE1976.LAB
-- >>> import Graphics.Color.Illuminant.CIE1931
-- >>> color (SVG :: SVG "aqua") :: Color (LAB 'D65) Float
-- <LAB CIE1931 'D65:(91.11637000,-48.08154700,-14.12457300)>
--
data SVG (n :: Symbol) = SVG

instance KnownSymbol n => Show (SVG (n :: Symbol)) where
  showsPrec n c
    | n == 0 = p . (symbolVal c ++)
    | otherwise = ('(' :) . p . (symbolVal c ++) . (')' :)
    where
      p = ("SVG " ++)

-- | Source: https://www.w3.org/TR/SVG11/types.html#ColorKeywords
--
-- @since 0.3.3
allSVGColors :: [(String, Color (SRGB 'NonLinear) Word8)]
allSVGColors =
  [ ("aliceblue", aliceBlue)
  , ("antiquewhite", antiqueWhite)
  , ("aqua", aqua)
  , ("aquamarine", aquaMarine)
  , ("azure", azure)
  , ("beige", beige)
  , ("bisque", bisque)
  , ("black", black)
  , ("blanchedalmond", blanchedAlmond)
  , ("blue", blue)
  , ("blueviolet", blueViolet)
  , ("brown", brown)
  , ("burlywood", burlyWood)
  , ("cadetblue", cadetBlue)
  , ("chartreuse", chartreuse)
  , ("chocolate", chocolate)
  , ("coral", coral)
  , ("cornflowerblue", cornFlowerBlue)
  , ("cornsilk", cornSilk)
  , ("crimson", crimson)
  , ("cyan", cyan)
  , ("darkblue", darkBlue)
  , ("darkcyan", darkCyan)
  , ("darkgoldenrod", darkGoldenRod)
  , ("darkgray", darkGray)
  , ("darkgreen", darkGreen)
  , ("darkgrey", darkGrey)
  , ("darkkhaki", darkKhaki)
  , ("darkmagenta", darkMagenta)
  , ("darkolivegreen", darkOliveGreen)
  , ("darkorange", darkOrange)
  , ("darkorchid", darkOrchid)
  , ("darkred", darkRed)
  , ("darksalmon", darkSalmon)
  , ("darkseagreen", darkSeaGreen)
  , ("darkslateblue", darkSlateBlue)
  , ("darkslategray", darkSlateGray)
  , ("darkslategrey", darkSlateGrey)
  , ("darkturquoise", darkTurquoise)
  , ("darkviolet", darkViolet)
  , ("deeppink", deepPink)
  , ("deepskyblue", deepSkyBlue)
  , ("dimgray", dimGray)
  , ("dimgrey", dimGrey)
  , ("dodgerblue", dodgerBlue)
  , ("firebrick", fireBrick)
  , ("floralwhite", floralWhite)
  , ("forestgreen", forestGreen)
  , ("fuchsia", fuchsia)
  , ("gainsboro", gainsboro)
  , ("ghostwhite", ghostWhite)
  , ("gold", gold)
  , ("goldenrod", goldenRod)
  , ("gray", gray)
  , ("grey", grey)
  , ("green", green)
  , ("greenyellow", greenYellow)
  , ("honeydew", honeydew)
  , ("hotpink", hotPink)
  , ("indianred", indianRed)
  , ("indigo", indigo)
  , ("ivory", ivory)
  , ("khaki", khaki)
  , ("lavender", lavender)
  , ("lavenderblush", lavenderBlush)
  , ("lawngreen", lawnGreen)
  , ("lemonchiffon", lemonChiffon)
  , ("lightblue", lightBlue)
  , ("lightcoral", lightCoral)
  , ("lightcyan", lightCyan)
  , ("lightgoldenrodyellow", lightGoldenRodYellow)
  , ("lightgray", lightGray)
  , ("lightgreen", lightGreen)
  , ("lightgrey", lightGrey)
  , ("lightpink", lightPink)
  , ("lightsalmon", lightSalmon)
  , ("lightseagreen", lightSeaGreen)
  , ("lightskyblue", lightSkyBlue)
  , ("lightslategray", lightSlateGray)
  , ("lightslategrey", lightSlateGrey)
  , ("lightsteelblue", lightSteelBlue)
  , ("lightyellow", lightYellow)
  , ("lime", lime)
  , ("limegreen", limeGreen)
  , ("linen", linen)
  , ("magenta", magenta)
  , ("maroon", maroon)
  , ("mediumaquamarine", mediumAquaMarine)
  , ("mediumblue", mediumBlue)
  , ("mediumorchid", mediumOrchid)
  , ("mediumpurple", mediumPurple)
  , ("mediumseagreen", mediumSeaGreen)
  , ("mediumslateblue", mediumSlateBlue)
  , ("mediumspringgreen", mediumSpringGreen)
  , ("mediumturquoise", mediumTurquoise)
  , ("mediumvioletred", mediumVioletRed)
  , ("midnightblue", midnightBlue)
  , ("mintcream", mintCream)
  , ("mistyrose", mistyRose)
  , ("moccasin", moccasin)
  , ("navajowhite", navajoWhite)
  , ("navy", navy)
  , ("oldlace", oldLace)
  , ("olive", olive)
  , ("olivedrab", oliveDrab)
  , ("orange", orange)
  , ("orangered", orangeRed)
  , ("orchid", orchid)
  , ("palegoldenrod", paleGoldenRod)
  , ("palegreen", paleGreen)
  , ("paleturquoise", paleTurquoise)
  , ("palevioletred", paleVioletRed)
  , ("papayawhip", papayaWhip)
  , ("peachpuff", peachPuff)
  , ("peru", peru)
  , ("pink", pink)
  , ("plum", plum)
  , ("powderblue", powderBlue)
  , ("purple", purple)
  , ("red", red)
  , ("rosybrown", rosyBrown)
  , ("royalblue", royalBlue)
  , ("saddlebrown", saddleBrown)
  , ("salmon", salmon)
  , ("sandybrown", sandyBrown)
  , ("seagreen", seaGreen)
  , ("seashell", seashell)
  , ("sienna", sienna)
  , ("silver", silver)
  , ("skyblue", skyBlue)
  , ("slateblue", slateBlue)
  , ("slategray", slateGray)
  , ("slategrey", slateGrey)
  , ("snow", snow)
  , ("springgreen", springGreen)
  , ("steelblue", steelBlue)
  , ("tan", tan)
  , ("teal", teal)
  , ("thistle", thistle)
  , ("tomato", tomato)
  , ("turquoise", turquoise)
  , ("violet", violet)
  , ("wheat", wheat)
  , ("white", white)
  , ("whitesmoke", whiteSmoke)
  , ("yellow", yellow)
  , ("yellowgreen", yellowGreen)
  ]

-- | <<files/svg/AliceBlue.png>>
instance StandardColor SVG "aliceblue" where color _ = convert aliceBlue

-- | <<files/svg/AntiqueWhite.png>>
instance StandardColor SVG "antiquewhite" where color _ = convert antiqueWhite

-- | <<files/svg/Aqua.png>>
instance StandardColor SVG "aqua" where color _ = convert aqua

-- | <<files/svg/AquaMarine.png>>
instance StandardColor SVG "aquamarine" where color _ = convert aquaMarine

-- | <<files/svg/Azure.png>>
instance StandardColor SVG "azure" where color _ = convert azure

-- | <<files/svg/Beige.png>>
instance StandardColor SVG "beige" where color _ = convert beige

-- | <<files/svg/Bisque.png>>
instance StandardColor SVG "bisque" where color _ = convert bisque

-- | <<files/svg/Black.png>>
instance StandardColor SVG "black" where color _ = convert black

-- | <<files/svg/BlanchedAlmond.png>>
instance StandardColor SVG "blanchedalmond" where color _ = convert blanchedAlmond

-- | <<files/svg/Blue.png>>
instance StandardColor SVG "blue" where color _ = convert blue

-- | <<files/svg/BlueViolet.png>>
instance StandardColor SVG "blueviolet" where color _ = convert blueViolet

-- | <<files/svg/Brown.png>>
instance StandardColor SVG "brown" where color _ = convert brown

-- | <<files/svg/BurlyWood.png>>
instance StandardColor SVG "burlywood" where color _ = convert burlyWood

-- | <<files/svg/CadetBlue.png>>
instance StandardColor SVG "cadetblue" where color _ = convert cadetBlue

-- | <<files/svg/Chartreuse.png>>
instance StandardColor SVG "chartreuse" where color _ = convert chartreuse

-- | <<files/svg/Chocolate.png>>
instance StandardColor SVG "chocolate" where color _ = convert chocolate

-- | <<files/svg/Coral.png>>
instance StandardColor SVG "coral" where color _ = convert coral

-- | <<files/svg/CornFlowerBlue.png>>
instance StandardColor SVG "cornflowerblue" where color _ = convert cornFlowerBlue

-- | <<files/svg/CornSilk.png>>
instance StandardColor SVG "cornsilk" where color _ = convert cornSilk

-- | <<files/svg/Crimson.png>>
instance StandardColor SVG "crimson" where color _ = convert crimson

-- | <<files/svg/Cyan.png>>
instance StandardColor SVG "cyan" where color _ = convert cyan

-- | <<files/svg/DarkBlue.png>>
instance StandardColor SVG "darkblue" where color _ = convert darkBlue

-- | <<files/svg/DarkCyan.png>>
instance StandardColor SVG "darkcyan" where color _ = convert darkCyan

-- | <<files/svg/DarkGoldenRod.png>>
instance StandardColor SVG "darkgoldenrod" where color _ = convert darkGoldenRod

-- | <<files/svg/DarkGray.png>>
instance StandardColor SVG "darkgray" where color _ = convert darkGray

-- | <<files/svg/DarkGreen.png>>
instance StandardColor SVG "darkgreen" where color _ = convert darkGreen

-- | <<files/svg/DarkGrey.png>>
instance StandardColor SVG "darkgrey" where color _ = convert darkGrey

-- | <<files/svg/DarkKhaki.png>>
instance StandardColor SVG "darkkhaki" where color _ = convert darkKhaki

-- | <<files/svg/DarkMagenta.png>>
instance StandardColor SVG "darkmagenta" where color _ = convert darkMagenta

-- | <<files/svg/DarkOliveGreen.png>>
instance StandardColor SVG "darkolivegreen" where color _ = convert darkOliveGreen

-- | <<files/svg/DarkOrange.png>>
instance StandardColor SVG "darkorange" where color _ = convert darkOrange

-- | <<files/svg/DarkOrchid.png>>
instance StandardColor SVG "darkorchid" where color _ = convert darkOrchid

-- | <<files/svg/DarkRed.png>>
instance StandardColor SVG "darkred" where color _ = convert darkRed

-- | <<files/svg/DarkSalmon.png>>
instance StandardColor SVG "darksalmon" where color _ = convert darkSalmon

-- | <<files/svg/DarkSeaGreen.png>>
instance StandardColor SVG "darkseagreen" where color _ = convert darkSeaGreen

-- | <<files/svg/DarkSlateBlue.png>>
instance StandardColor SVG "darkslateblue" where color _ = convert darkSlateBlue

-- | <<files/svg/DarkSlateGray.png>>
instance StandardColor SVG "darkslategray" where color _ = convert darkSlateGray

-- | <<files/svg/DarkSlateGrey.png>>
instance StandardColor SVG "darkslategrey" where color _ = convert darkSlateGrey

-- | <<files/svg/DarkTurquoise.png>>
instance StandardColor SVG "darkturquoise" where color _ = convert darkTurquoise

-- | <<files/svg/DarkViolet.png>>
instance StandardColor SVG "darkviolet" where color _ = convert darkViolet

-- | <<files/svg/DeepPink.png>>
instance StandardColor SVG "deeppink" where color _ = convert deepPink

-- | <<files/svg/DeepSkyBlue.png>>
instance StandardColor SVG "deepskyblue" where color _ = convert deepSkyBlue

-- | <<files/svg/DimGray.png>>
instance StandardColor SVG "dimgray" where color _ = convert dimGray

-- | <<files/svg/DimGrey.png>>
instance StandardColor SVG "dimgrey" where color _ = convert dimGrey

-- | <<files/svg/DodgerBlue.png>>
instance StandardColor SVG "dodgerblue" where color _ = convert dodgerBlue

-- | <<files/svg/FireBrick.png>>
instance StandardColor SVG "firebrick" where color _ = convert fireBrick

-- | <<files/svg/FloralWhite.png>>
instance StandardColor SVG "floralwhite" where color _ = convert floralWhite

-- | <<files/svg/ForestGreen.png>>
instance StandardColor SVG "forestgreen" where color _ = convert forestGreen

-- | <<files/svg/Fuchsia.png>>
instance StandardColor SVG "fuchsia" where color _ = convert fuchsia

-- | <<files/svg/Gainsboro.png>>
instance StandardColor SVG "gainsboro" where color _ = convert gainsboro

-- | <<files/svg/GhostWhite.png>>
instance StandardColor SVG "ghostwhite" where color _ = convert ghostWhite

-- | <<files/svg/Gold.png>>
instance StandardColor SVG "gold" where color _ = convert gold

-- | <<files/svg/GoldenRod.png>>
instance StandardColor SVG "goldenrod" where color _ = convert goldenRod

-- | <<files/svg/Gray.png>>
instance StandardColor SVG "gray" where color _ = convert gray

-- | <<files/svg/Grey.png>>
instance StandardColor SVG "grey" where color _ = convert grey

-- | <<files/svg/Green.png>>
instance StandardColor SVG "green" where color _ = convert green

-- | <<files/svg/GreenYellow.png>>
instance StandardColor SVG "greenyellow" where color _ = convert greenYellow

-- | <<files/svg/Honeydew.png>>
instance StandardColor SVG "honeydew" where color _ = convert honeydew

-- | <<files/svg/HotPink.png>>
instance StandardColor SVG "hotpink" where color _ = convert hotPink

-- | <<files/svg/IndianRed.png>>
instance StandardColor SVG "indianred" where color _ = convert indianRed

-- | <<files/svg/Indigo.png>>
instance StandardColor SVG "indigo" where color _ = convert indigo

-- | <<files/svg/Ivory.png>>
instance StandardColor SVG "ivory" where color _ = convert ivory

-- | <<files/svg/Khaki.png>>
instance StandardColor SVG "khaki" where color _ = convert khaki

-- | <<files/svg/Lavender.png>>
instance StandardColor SVG "lavender" where color _ = convert lavender

-- | <<files/svg/LavenderBlush.png>>
instance StandardColor SVG "lavenderblush" where color _ = convert lavenderBlush

-- | <<files/svg/LawnGreen.png>>
instance StandardColor SVG "lawngreen" where color _ = convert lawnGreen

-- | <<files/svg/LemonChiffon.png>>
instance StandardColor SVG "lemonchiffon" where color _ = convert lemonChiffon

-- | <<files/svg/LightBlue.png>>
instance StandardColor SVG "lightblue" where color _ = convert lightBlue

-- | <<files/svg/LightCoral.png>>
instance StandardColor SVG "lightcoral" where color _ = convert lightCoral

-- | <<files/svg/LightCyan.png>>
instance StandardColor SVG "lightcyan" where color _ = convert lightCyan

-- | <<files/svg/LightGoldenRodYellow.png>>
instance StandardColor SVG "lightgoldenrodyellow" where color _ = convert lightGoldenRodYellow

-- | <<files/svg/LightGray.png>>
instance StandardColor SVG "lightgray" where color _ = convert lightGray

-- | <<files/svg/LightGreen.png>>
instance StandardColor SVG "lightgreen" where color _ = convert lightGreen

-- | <<files/svg/LightGrey.png>>
instance StandardColor SVG "lightgrey" where color _ = convert lightGrey

-- | <<files/svg/LightPink.png>>
instance StandardColor SVG "lightpink" where color _ = convert lightPink

-- | <<files/svg/LightSalmon.png>>
instance StandardColor SVG "lightsalmon" where color _ = convert lightSalmon

-- | <<files/svg/LightSeaGreen.png>>
instance StandardColor SVG "lightseagreen" where color _ = convert lightSeaGreen

-- | <<files/svg/LightSkyBlue.png>>
instance StandardColor SVG "lightskyblue" where color _ = convert lightSkyBlue

-- | <<files/svg/LightSlateGray.png>>
instance StandardColor SVG "lightslategray" where color _ = convert lightSlateGray

-- | <<files/svg/LightSlateGrey.png>>
instance StandardColor SVG "lightslategrey" where color _ = convert lightSlateGrey

-- | <<files/svg/LightSteelBlue.png>>
instance StandardColor SVG "lightsteelblue" where color _ = convert lightSteelBlue

-- | <<files/svg/LightYellow.png>>
instance StandardColor SVG "lightyellow" where color _ = convert lightYellow

-- | <<files/svg/Lime.png>>
instance StandardColor SVG "lime" where color _ = convert lime

-- | <<files/svg/LimeGreen.png>>
instance StandardColor SVG "limegreen" where color _ = convert limeGreen

-- | <<files/svg/Linen.png>>
instance StandardColor SVG "linen" where color _ = convert linen

-- | <<files/svg/Magenta.png>>
instance StandardColor SVG "magenta" where color _ = convert magenta

-- | <<files/svg/Maroon.png>>
instance StandardColor SVG "maroon" where color _ = convert maroon

-- | <<files/svg/MediumAquaMarine.png>>
instance StandardColor SVG "mediumaquamarine" where color _ = convert mediumAquaMarine

-- | <<files/svg/MediumBlue.png>>
instance StandardColor SVG "mediumblue" where color _ = convert mediumBlue

-- | <<files/svg/MediumOrchid.png>>
instance StandardColor SVG "mediumorchid" where color _ = convert mediumOrchid

-- | <<files/svg/MediumPurple.png>>
instance StandardColor SVG "mediumpurple" where color _ = convert mediumPurple

-- | <<files/svg/MediumSeaGreen.png>>
instance StandardColor SVG "mediumseagreen" where color _ = convert mediumSeaGreen

-- | <<files/svg/MediumSlateBlue.png>>
instance StandardColor SVG "mediumslateblue" where color _ = convert mediumSlateBlue

-- | <<files/svg/MediumSpringGreen.png>>
instance StandardColor SVG "mediumspringgreen" where color _ = convert mediumSpringGreen

-- | <<files/svg/MediumTurquoise.png>>
instance StandardColor SVG "mediumturquoise" where color _ = convert mediumTurquoise

-- | <<files/svg/MediumVioletRed.png>>
instance StandardColor SVG "mediumvioletred" where color _ = convert mediumVioletRed

-- | <<files/svg/MidnightBlue.png>>
instance StandardColor SVG "midnightblue" where color _ = convert midnightBlue

-- | <<files/svg/MintCream.png>>
instance StandardColor SVG "mintcream" where color _ = convert mintCream

-- | <<files/svg/MistyRose.png>>
instance StandardColor SVG "mistyrose" where color _ = convert mistyRose

-- | <<files/svg/Moccasin.png>>
instance StandardColor SVG "moccasin" where color _ = convert moccasin

-- | <<files/svg/NavajoWhite.png>>
instance StandardColor SVG "navajowhite" where color _ = convert navajoWhite

-- | <<files/svg/Navy.png>>
instance StandardColor SVG "navy" where color _ = convert navy

-- | <<files/svg/OldLace.png>>
instance StandardColor SVG "oldlace" where color _ = convert oldLace

-- | <<files/svg/Olive.png>>
instance StandardColor SVG "olive" where color _ = convert olive

-- | <<files/svg/OliveDrab.png>>
instance StandardColor SVG "olivedrab" where color _ = convert oliveDrab

-- | <<files/svg/Orange.png>>
instance StandardColor SVG "orange" where color _ = convert orange

-- | <<files/svg/OrangeRed.png>>
instance StandardColor SVG "orangered" where color _ = convert orangeRed

-- | <<files/svg/Orchid.png>>
instance StandardColor SVG "orchid" where color _ = convert orchid

-- | <<files/svg/PaleGoldenRod.png>>
instance StandardColor SVG "palegoldenrod" where color _ = convert paleGoldenRod

-- | <<files/svg/PaleGreen.png>>
instance StandardColor SVG "palegreen" where color _ = convert paleGreen

-- | <<files/svg/PaleTurquoise.png>>
instance StandardColor SVG "paleturquoise" where color _ = convert paleTurquoise

-- | <<files/svg/PaleVioletRed.png>>
instance StandardColor SVG "palevioletred" where color _ = convert paleVioletRed

-- | <<files/svg/PapayaWhip.png>>
instance StandardColor SVG "papayawhip" where color _ = convert papayaWhip

-- | <<files/svg/PeachPuff.png>>
instance StandardColor SVG "peachpuff" where color _ = convert peachPuff

-- | <<files/svg/Peru.png>>
instance StandardColor SVG "peru" where color _ = convert peru

-- | <<files/svg/Pink.png>>
instance StandardColor SVG "pink" where color _ = convert pink

-- | <<files/svg/Plum.png>>
instance StandardColor SVG "plum" where color _ = convert plum

-- | <<files/svg/PowderBlue.png>>
instance StandardColor SVG "powderblue" where color _ = convert powderBlue

-- | <<files/svg/Purple.png>>
instance StandardColor SVG "purple" where color _ = convert purple

-- | <<files/svg/Red.png>>
instance StandardColor SVG "red" where color _ = convert red

-- | <<files/svg/RosyBrown.png>>
instance StandardColor SVG "rosybrown" where color _ = convert rosyBrown

-- | <<files/svg/RoyalBlue.png>>
instance StandardColor SVG "royalblue" where color _ = convert royalBlue

-- | <<files/svg/SaddleBrown.png>>
instance StandardColor SVG "saddlebrown" where color _ = convert saddleBrown

-- | <<files/svg/Salmon.png>>
instance StandardColor SVG "salmon" where color _ = convert salmon

-- | <<files/svg/SandyBrown.png>>
instance StandardColor SVG "sandybrown" where color _ = convert sandyBrown

-- | <<files/svg/SeaGreen.png>>
instance StandardColor SVG "seagreen" where color _ = convert seaGreen

-- | <<files/svg/Seashell.png>>
instance StandardColor SVG "seashell" where color _ = convert seashell

-- | <<files/svg/Sienna.png>>
instance StandardColor SVG "sienna" where color _ = convert sienna

-- | <<files/svg/Silver.png>>
instance StandardColor SVG "silver" where color _ = convert silver

-- | <<files/svg/SkyBlue.png>>
instance StandardColor SVG "skyblue" where color _ = convert skyBlue

-- | <<files/svg/SlateBlue.png>>
instance StandardColor SVG "slateblue" where color _ = convert slateBlue

-- | <<files/svg/SlateGray.png>>
instance StandardColor SVG "slategray" where color _ = convert slateGray

-- | <<files/svg/SlateGrey.png>>
instance StandardColor SVG "slategrey" where color _ = convert slateGrey

-- | <<files/svg/Snow.png>>
instance StandardColor SVG "snow" where color _ = convert snow

-- | <<files/svg/SpringGreen.png>>
instance StandardColor SVG "springgreen" where color _ = convert springGreen

-- | <<files/svg/SteelBlue.png>>
instance StandardColor SVG "steelblue" where color _ = convert steelBlue

-- | <<files/svg/Tan.png>>
instance StandardColor SVG "tan" where color _ = convert tan

-- | <<files/svg/Teal.png>>
instance StandardColor SVG "teal" where color _ = convert teal

-- | <<files/svg/Thistle.png>>
instance StandardColor SVG "thistle" where color _ = convert thistle

-- | <<files/svg/Tomato.png>>
instance StandardColor SVG "tomato" where color _ = convert tomato

-- | <<files/svg/Turquoise.png>>
instance StandardColor SVG "turquoise" where color _ = convert turquoise

-- | <<files/svg/Violet.png>>
instance StandardColor SVG "violet" where color _ = convert violet

-- | <<files/svg/Wheat.png>>
instance StandardColor SVG "wheat" where color _ = convert wheat

-- | <<files/svg/White.png>>
instance StandardColor SVG "white" where color _ = convert white

-- | <<files/svg/WhiteSmoke.png>>
instance StandardColor SVG "whitesmoke" where color _ = convert whiteSmoke

-- | <<files/svg/Yellow.png>>
instance StandardColor SVG "yellow" where color _ = convert yellow

-- | <<files/svg/YellowGreen.png>>
instance StandardColor SVG "yellowgreen" where color _ = convert yellowGreen

-- | Defined in SVG1.1 as
--
-- @
-- aliceblue = rgb(240, 248, 255)
-- @
--
-- <<files/svg/AliceBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = aliceBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/AliceBlue.png" img
--
-- @since 0.3.3
aliceBlue :: Color (SRGB 'NonLinear) Word8
aliceBlue = ColorSRGB 240 248 255

-- | Defined in SVG1.1 as
--
-- @
-- antiquewhite = rgb(250, 235, 215)
-- @
--
-- <<files/svg/AntiqueWhite.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = antiqueWhite
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/AntiqueWhite.png" img
--
-- @since 0.3.3
antiqueWhite :: Color (SRGB 'NonLinear) Word8
antiqueWhite = ColorSRGB 250 235 215

-- | Defined in SVG1.1 as
--
-- @
-- aqua = rgb(0, 255, 255)
-- @
--
-- <<files/svg/Aqua.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = aqua
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Aqua.png" img
--
-- @since 0.3.3
aqua :: Color (SRGB 'NonLinear) Word8
aqua = ColorSRGB 0 255 255

-- | Defined in SVG1.1 as
--
-- @
-- aquamarine = rgb(127, 255, 212)
-- @
--
-- <<files/svg/AquaMarine.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = aquaMarine
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/AquaMarine.png" img
--
-- @since 0.3.3
aquaMarine :: Color (SRGB 'NonLinear) Word8
aquaMarine = ColorSRGB 127 255 212

-- | Defined in SVG1.1 as
--
-- @
-- azure = rgb(240, 255, 255)
-- @
--
-- <<files/svg/Azure.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = azure
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Azure.png" img
--
-- @since 0.3.3
azure :: Color (SRGB 'NonLinear) Word8
azure = ColorSRGB 240 255 255

-- | Defined in SVG1.1 as
--
-- @
-- beige = rgb(245, 245, 220)
-- @
--
-- <<files/svg/Beige.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = beige
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Beige.png" img
--
-- @since 0.3.3
beige :: Color (SRGB 'NonLinear) Word8
beige = ColorSRGB 245 245 220

-- | Defined in SVG1.1 as
--
-- @
-- bisque = rgb(255, 228, 196)
-- @
--
-- <<files/svg/Bisque.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = bisque
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Bisque.png" img
--
-- @since 0.3.3
bisque :: Color (SRGB 'NonLinear) Word8
bisque = ColorSRGB 255 228 196

-- | Defined in SVG1.1 as
--
-- @
-- black = rgb(0, 0, 0)
-- @
--
-- <<files/svg/Black.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = black
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Black.png" img
--
-- @since 0.3.3
black :: Color (SRGB 'NonLinear) Word8
black = ColorSRGB 0 0 0

-- | Defined in SVG1.1 as
--
-- @
-- blanchedalmond = rgb(255, 235, 205)
-- @
--
-- <<files/svg/BlanchedAlmond.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = blanchedAlmond
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/BlanchedAlmond.png" img
--
-- @since 0.3.3
blanchedAlmond :: Color (SRGB 'NonLinear) Word8
blanchedAlmond = ColorSRGB 255 235 205

-- | Defined in SVG1.1 as
--
-- @
-- blue = rgb(0, 0, 255)
-- @
--
-- <<files/svg/Blue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = blue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Blue.png" img
--
-- @since 0.3.3
blue :: Color (SRGB 'NonLinear) Word8
blue = ColorSRGB 0 0 255

-- | Defined in SVG1.1 as
--
-- @
-- blueviolet = rgb(138, 43, 226)
-- @
--
-- <<files/svg/BlueViolet.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = blueViolet
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/BlueViolet.png" img
--
-- @since 0.3.3
blueViolet :: Color (SRGB 'NonLinear) Word8
blueViolet = ColorSRGB 138 43 226

-- | Defined in SVG1.1 as
--
-- @
-- brown = rgb(165, 42, 42)
-- @
--
-- <<files/svg/Brown.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = brown
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Brown.png" img
--
-- @since 0.3.3
brown :: Color (SRGB 'NonLinear) Word8
brown = ColorSRGB 165 42 42

-- | Defined in SVG1.1 as
--
-- @
-- burlywood = rgb(222, 184, 135)
-- @
--
-- <<files/svg/BurlyWood.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = burlyWood
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/BurlyWood.png" img
--
-- @since 0.3.3
burlyWood :: Color (SRGB 'NonLinear) Word8
burlyWood = ColorSRGB 222 184 135

-- | Defined in SVG1.1 as
--
-- @
-- cadetblue = rgb(95, 158, 160)
-- @
--
-- <<files/svg/CadetBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = cadetBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/CadetBlue.png" img
--
-- @since 0.3.3
cadetBlue :: Color (SRGB 'NonLinear) Word8
cadetBlue = ColorSRGB 95 158 160

-- | Defined in SVG1.1 as
--
-- @
-- chartreuse = rgb(127, 255, 0)
-- @
--
-- <<files/svg/Chartreuse.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = chartreuse
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Chartreuse.png" img
--
-- @since 0.3.3
chartreuse :: Color (SRGB 'NonLinear) Word8
chartreuse = ColorSRGB 127 255 0

-- | Defined in SVG1.1 as
--
-- @
-- chocolate = rgb(210, 105, 30)
-- @
--
-- <<files/svg/Chocolate.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = chocolate
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Chocolate.png" img
--
-- @since 0.3.3
chocolate :: Color (SRGB 'NonLinear) Word8
chocolate = ColorSRGB 210 105 30

-- | Defined in SVG1.1 as
--
-- @
-- coral = rgb(255, 127, 80)
-- @
--
-- <<files/svg/Coral.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = coral
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Coral.png" img
--
-- @since 0.3.3
coral :: Color (SRGB 'NonLinear) Word8
coral = ColorSRGB 255 127 80

-- | Defined in SVG1.1 as
--
-- @
-- cornflowerblue = rgb(100, 149, 237)
-- @
--
-- <<files/svg/CornFlowerBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = cornFlowerBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/CornFlowerBlue.png" img
--
-- @since 0.3.3
cornFlowerBlue :: Color (SRGB 'NonLinear) Word8
cornFlowerBlue = ColorSRGB 100 149 237

-- | Defined in SVG1.1 as
--
-- @
-- cornsilk = rgb(255, 248, 220)
-- @
--
-- <<files/svg/CornSilk.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = cornSilk
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/CornSilk.png" img
--
-- @since 0.3.3
cornSilk :: Color (SRGB 'NonLinear) Word8
cornSilk = ColorSRGB 255 248 220

-- | Defined in SVG1.1 as
--
-- @
-- crimson = rgb(220, 20, 60)
-- @
--
-- <<files/svg/Crimson.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = crimson
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Crimson.png" img
--
-- @since 0.3.3
crimson :: Color (SRGB 'NonLinear) Word8
crimson = ColorSRGB 220 20 60

-- | Defined in SVG1.1 as
--
-- @
-- cyan = rgb(0, 255, 255)
-- @
--
-- <<files/svg/Cyan.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = cyan
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Cyan.png" img
--
-- @since 0.3.3
cyan :: Color (SRGB 'NonLinear) Word8
cyan = ColorSRGB 0 255 255

-- | Defined in SVG1.1 as
--
-- @
-- darkblue = rgb(0, 0, 139)
-- @
--
-- <<files/svg/DarkBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkBlue.png" img
--
-- @since 0.3.3
darkBlue :: Color (SRGB 'NonLinear) Word8
darkBlue = ColorSRGB 0 0 139

-- | Defined in SVG1.1 as
--
-- @
-- darkcyan = rgb(0, 139, 139)
-- @
--
-- <<files/svg/DarkCyan.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkCyan
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkCyan.png" img
--
-- @since 0.3.3
darkCyan :: Color (SRGB 'NonLinear) Word8
darkCyan = ColorSRGB 0 139 139

-- | Defined in SVG1.1 as
--
-- @
-- darkgoldenrod = rgb(184, 134, 11)
-- @
--
-- <<files/svg/DarkGoldenRod.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkGoldenRod
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkGoldenRod.png" img
--
-- @since 0.3.3
darkGoldenRod :: Color (SRGB 'NonLinear) Word8
darkGoldenRod = ColorSRGB 184 134 11

-- | Defined in SVG1.1 as
--
-- @
-- darkgray = rgb(169, 169, 169)
-- @
--
-- <<files/svg/DarkGray.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkGray
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkGray.png" img
--
-- @since 0.3.3
darkGray :: Color (SRGB 'NonLinear) Word8
darkGray = ColorSRGB 169 169 169

-- | Defined in SVG1.1 as
--
-- @
-- darkgreen = rgb(0, 100, 0)
-- @
--
-- <<files/svg/DarkGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkGreen.png" img
--
-- @since 0.3.3
darkGreen :: Color (SRGB 'NonLinear) Word8
darkGreen = ColorSRGB 0 100 0

-- | Defined in SVG1.1 as
--
-- @
-- darkgrey = rgb(169, 169, 169)
-- @
--
-- <<files/svg/DarkGrey.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkGrey
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkGrey.png" img
--
-- @since 0.3.3
darkGrey :: Color (SRGB 'NonLinear) Word8
darkGrey = ColorSRGB 169 169 169

-- | Defined in SVG1.1 as
--
-- @
-- darkkhaki = rgb(189, 183, 107)
-- @
--
-- <<files/svg/DarkKhaki.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkKhaki
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkKhaki.png" img
--
-- @since 0.3.3
darkKhaki :: Color (SRGB 'NonLinear) Word8
darkKhaki = ColorSRGB 189 183 107

-- | Defined in SVG1.1 as
--
-- @
-- darkmagenta = rgb(139, 0, 139)
-- @
--
-- <<files/svg/DarkMagenta.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkMagenta
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkMagenta.png" img
--
-- @since 0.3.3
darkMagenta :: Color (SRGB 'NonLinear) Word8
darkMagenta = ColorSRGB 139 0 139

-- | Defined in SVG1.1 as
--
-- @
-- darkolivegreen = rgb(85, 107, 47)
-- @
--
-- <<files/svg/DarkOliveGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkOliveGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkOliveGreen.png" img
--
-- @since 0.3.3
darkOliveGreen :: Color (SRGB 'NonLinear) Word8
darkOliveGreen = ColorSRGB 85 107 47

-- | Defined in SVG1.1 as
--
-- @
-- darkorange = rgb(255, 140, 0)
-- @
--
-- <<files/svg/DarkOrange.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkOrange
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkOrange.png" img
--
-- @since 0.3.3
darkOrange :: Color (SRGB 'NonLinear) Word8
darkOrange = ColorSRGB 255 140 0

-- | Defined in SVG1.1 as
--
-- @
-- darkorchid = rgb(153, 50, 204)
-- @
--
-- <<files/svg/DarkOrchid.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkOrchid
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkOrchid.png" img
--
-- @since 0.3.3
darkOrchid :: Color (SRGB 'NonLinear) Word8
darkOrchid = ColorSRGB 153 50 204

-- | Defined in SVG1.1 as
--
-- @
-- darkred = rgb(139, 0, 0)
-- @
--
-- <<files/svg/DarkRed.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkRed
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkRed.png" img
--
-- @since 0.3.3
darkRed :: Color (SRGB 'NonLinear) Word8
darkRed = ColorSRGB 139 0 0

-- | Defined in SVG1.1 as
--
-- @
-- darksalmon = rgb(233, 150, 122)
-- @
--
-- <<files/svg/DarkSalmon.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkSalmon
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkSalmon.png" img
--
-- @since 0.3.3
darkSalmon :: Color (SRGB 'NonLinear) Word8
darkSalmon = ColorSRGB 233 150 122

-- | Defined in SVG1.1 as
--
-- @
-- darkseagreen = rgb(143, 188, 143)
-- @
--
-- <<files/svg/DarkSeaGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkSeaGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkSeaGreen.png" img
--
-- @since 0.3.3
darkSeaGreen :: Color (SRGB 'NonLinear) Word8
darkSeaGreen = ColorSRGB 143 188 143

-- | Defined in SVG1.1 as
--
-- @
-- darkslateblue = rgb(72, 61, 139)
-- @
--
-- <<files/svg/DarkSlateBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkSlateBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkSlateBlue.png" img
--
-- @since 0.3.3
darkSlateBlue :: Color (SRGB 'NonLinear) Word8
darkSlateBlue = ColorSRGB 72 61 139

-- | Defined in SVG1.1 as
--
-- @
-- darkslategray = rgb(47, 79, 79)
-- @
--
-- <<files/svg/DarkSlateGray.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkSlateGray
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkSlateGray.png" img
--
-- @since 0.3.3
darkSlateGray :: Color (SRGB 'NonLinear) Word8
darkSlateGray = ColorSRGB 47 79 79

-- | Defined in SVG1.1 as
--
-- @
-- darkslategrey = rgb(47, 79, 79)
-- @
--
-- <<files/svg/DarkSlateGrey.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkSlateGrey
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkSlateGrey.png" img
--
-- @since 0.3.3
darkSlateGrey :: Color (SRGB 'NonLinear) Word8
darkSlateGrey = ColorSRGB 47 79 79

-- | Defined in SVG1.1 as
--
-- @
-- darkturquoise = rgb(0, 206, 209)
-- @
--
-- <<files/svg/DarkTurquoise.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkTurquoise
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkTurquoise.png" img
--
-- @since 0.3.3
darkTurquoise :: Color (SRGB 'NonLinear) Word8
darkTurquoise = ColorSRGB 0 206 209

-- | Defined in SVG1.1 as
--
-- @
-- darkviolet = rgb(148, 0, 211)
-- @
--
-- <<files/svg/DarkViolet.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = darkViolet
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DarkViolet.png" img
--
-- @since 0.3.3
darkViolet :: Color (SRGB 'NonLinear) Word8
darkViolet = ColorSRGB 148 0 211

-- | Defined in SVG1.1 as
--
-- @
-- deeppink = rgb(255, 20, 147)
-- @
--
-- <<files/svg/DeepPink.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = deepPink
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DeepPink.png" img
--
-- @since 0.3.3
deepPink :: Color (SRGB 'NonLinear) Word8
deepPink = ColorSRGB 255 20 147

-- | Defined in SVG1.1 as
--
-- @
-- deepskyblue = rgb(0, 191, 255)
-- @
--
-- <<files/svg/DeepSkyBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = deepSkyBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DeepSkyBlue.png" img
--
-- @since 0.3.3
deepSkyBlue :: Color (SRGB 'NonLinear) Word8
deepSkyBlue = ColorSRGB 0 191 255

-- | Defined in SVG1.1 as
--
-- @
-- dimgray = rgb(105, 105, 105)
-- @
--
-- <<files/svg/DimGray.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = dimGray
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DimGray.png" img
--
-- @since 0.3.3
dimGray :: Color (SRGB 'NonLinear) Word8
dimGray = ColorSRGB 105 105 105

-- | Defined in SVG1.1 as
--
-- @
-- dimgrey = rgb(105, 105, 105)
-- @
--
-- <<files/svg/DimGrey.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = dimGrey
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DimGrey.png" img
--
-- @since 0.3.3
dimGrey :: Color (SRGB 'NonLinear) Word8
dimGrey = ColorSRGB 105 105 105

-- | Defined in SVG1.1 as
--
-- @
-- dodgerblue = rgb(30, 144, 255)
-- @
--
-- <<files/svg/DodgerBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = dodgerBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/DodgerBlue.png" img
--
-- @since 0.3.3
dodgerBlue :: Color (SRGB 'NonLinear) Word8
dodgerBlue = ColorSRGB 30 144 255

-- | Defined in SVG1.1 as
--
-- @
-- firebrick = rgb(178, 34, 34)
-- @
--
-- <<files/svg/FireBrick.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = fireBrick
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/FireBrick.png" img
--
-- @since 0.3.3
fireBrick :: Color (SRGB 'NonLinear) Word8
fireBrick = ColorSRGB 178 34 34

-- | Defined in SVG1.1 as
--
-- @
-- floralwhite = rgb(255, 250, 240)
-- @
--
-- <<files/svg/FloralWhite.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = floralWhite
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/FloralWhite.png" img
--
-- @since 0.3.3
floralWhite :: Color (SRGB 'NonLinear) Word8
floralWhite = ColorSRGB 255 250 240

-- | Defined in SVG1.1 as
--
-- @
-- forestgreen = rgb(34, 139, 34)
-- @
--
-- <<files/svg/ForestGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = forestGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/ForestGreen.png" img
--
-- @since 0.3.3
forestGreen :: Color (SRGB 'NonLinear) Word8
forestGreen = ColorSRGB 34 139 34

-- | Defined in SVG1.1 as
--
-- @
-- fuchsia = rgb(255, 0, 255)
-- @
--
-- <<files/svg/Fuchsia.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = fuchsia
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Fuchsia.png" img
--
-- @since 0.3.3
fuchsia :: Color (SRGB 'NonLinear) Word8
fuchsia = ColorSRGB 255 0 255

-- | Defined in SVG1.1 as
--
-- @
-- gainsboro = rgb(220, 220, 220)
-- @
--
-- <<files/svg/Gainsboro.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = gainsboro
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Gainsboro.png" img
--
-- @since 0.3.3
gainsboro :: Color (SRGB 'NonLinear) Word8
gainsboro = ColorSRGB 220 220 220

-- | Defined in SVG1.1 as
--
-- @
-- ghostwhite = rgb(248, 248, 255)
-- @
--
-- <<files/svg/GhostWhite.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = ghostWhite
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/GhostWhite.png" img
--
-- @since 0.3.3
ghostWhite :: Color (SRGB 'NonLinear) Word8
ghostWhite = ColorSRGB 248 248 255

-- | Defined in SVG1.1 as
--
-- @
-- gold = rgb(255, 215, 0)
-- @
--
-- <<files/svg/Gold.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = gold
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Gold.png" img
--
-- @since 0.3.3
gold :: Color (SRGB 'NonLinear) Word8
gold = ColorSRGB 255 215 0

-- | Defined in SVG1.1 as
--
-- @
-- goldenrod = rgb(218, 165, 32)
-- @
--
-- <<files/svg/GoldenRod.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = goldenRod
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/GoldenRod.png" img
--
-- @since 0.3.3
goldenRod :: Color (SRGB 'NonLinear) Word8
goldenRod = ColorSRGB 218 165 32

-- | Defined in SVG1.1 as
--
-- @
-- gray = rgb(128, 128, 128)
-- @
--
-- <<files/svg/Gray.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = gray
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Gray.png" img
--
-- @since 0.3.3
gray :: Color (SRGB 'NonLinear) Word8
gray = ColorSRGB 128 128 128

-- | Defined in SVG1.1 as
--
-- @
-- grey = rgb(128, 128, 128)
-- @
--
-- <<files/svg/Grey.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = grey
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Grey.png" img
--
-- @since 0.3.3
grey :: Color (SRGB 'NonLinear) Word8
grey = ColorSRGB 128 128 128

-- | Defined in SVG1.1 as
--
-- @
-- green = rgb(0, 128, 0)
-- @
--
-- <<files/svg/Green.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = green
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Green.png" img
--
-- @since 0.3.3
green :: Color (SRGB 'NonLinear) Word8
green = ColorSRGB 0 128 0

-- | Defined in SVG1.1 as
--
-- @
-- greenyellow = rgb(173, 255, 47)
-- @
--
-- <<files/svg/GreenYellow.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = greenYellow
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/GreenYellow.png" img
--
-- @since 0.3.3
greenYellow :: Color (SRGB 'NonLinear) Word8
greenYellow = ColorSRGB 173 255 47

-- | Defined in SVG1.1 as
--
-- @
-- honeydew = rgb(240, 255, 240)
-- @
--
-- <<files/svg/Honeydew.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = honeydew
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Honeydew.png" img
--
-- @since 0.3.3
honeydew :: Color (SRGB 'NonLinear) Word8
honeydew = ColorSRGB 240 255 240

-- | Defined in SVG1.1 as
--
-- @
-- hotpink = rgb(255, 105, 180)
-- @
--
-- <<files/svg/HotPink.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = hotPink
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/HotPink.png" img
--
-- @since 0.3.3
hotPink :: Color (SRGB 'NonLinear) Word8
hotPink = ColorSRGB 255 105 180

-- | Defined in SVG1.1 as
--
-- @
-- indianred = rgb(205, 92, 92)
-- @
--
-- <<files/svg/IndianRed.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = indianRed
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/IndianRed.png" img
--
-- @since 0.3.3
indianRed :: Color (SRGB 'NonLinear) Word8
indianRed = ColorSRGB 205 92 92

-- | Defined in SVG1.1 as
--
-- @
-- indigo = rgb(75, 0, 130)
-- @
--
-- <<files/svg/Indigo.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = indigo
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Indigo.png" img
--
-- @since 0.3.3
indigo :: Color (SRGB 'NonLinear) Word8
indigo = ColorSRGB 75 0 130

-- | Defined in SVG1.1 as
--
-- @
-- ivory = rgb(255, 255, 240)
-- @
--
-- <<files/svg/Ivory.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = ivory
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Ivory.png" img
--
-- @since 0.3.3
ivory :: Color (SRGB 'NonLinear) Word8
ivory = ColorSRGB 255 255 240

-- | Defined in SVG1.1 as
--
-- @
-- khaki = rgb(240, 230, 140)
-- @
--
-- <<files/svg/Khaki.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = khaki
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Khaki.png" img
--
-- @since 0.3.3
khaki :: Color (SRGB 'NonLinear) Word8
khaki = ColorSRGB 240 230 140

-- | Defined in SVG1.1 as
--
-- @
-- lavender = rgb(230, 230, 250)
-- @
--
-- <<files/svg/Lavender.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lavender
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Lavender.png" img
--
-- @since 0.3.3
lavender :: Color (SRGB 'NonLinear) Word8
lavender = ColorSRGB 230 230 250

-- | Defined in SVG1.1 as
--
-- @
-- lavenderblush = rgb(255, 240, 245)
-- @
--
-- <<files/svg/LavenderBlush.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lavenderBlush
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LavenderBlush.png" img
--
-- @since 0.3.3
lavenderBlush :: Color (SRGB 'NonLinear) Word8
lavenderBlush = ColorSRGB 255 240 245

-- | Defined in SVG1.1 as
--
-- @
-- lawngreen = rgb(124, 252, 0)
-- @
--
-- <<files/svg/LawnGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lawnGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LawnGreen.png" img
--
-- @since 0.3.3
lawnGreen :: Color (SRGB 'NonLinear) Word8
lawnGreen = ColorSRGB 124 252 0

-- | Defined in SVG1.1 as
--
-- @
-- lemonchiffon = rgb(255, 250, 205)
-- @
--
-- <<files/svg/LemonChiffon.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lemonChiffon
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LemonChiffon.png" img
--
-- @since 0.3.3
lemonChiffon :: Color (SRGB 'NonLinear) Word8
lemonChiffon = ColorSRGB 255 250 205

-- | Defined in SVG1.1 as
--
-- @
-- lightblue = rgb(173, 216, 230)
-- @
--
-- <<files/svg/LightBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightBlue.png" img
--
-- @since 0.3.3
lightBlue :: Color (SRGB 'NonLinear) Word8
lightBlue = ColorSRGB 173 216 230

-- | Defined in SVG1.1 as
--
-- @
-- lightcoral = rgb(240, 128, 128)
-- @
--
-- <<files/svg/LightCoral.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightCoral
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightCoral.png" img
--
-- @since 0.3.3
lightCoral :: Color (SRGB 'NonLinear) Word8
lightCoral = ColorSRGB 240 128 128

-- | Defined in SVG1.1 as
--
-- @
-- lightcyan = rgb(224, 255, 255)
-- @
--
-- <<files/svg/LightCyan.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightCyan
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightCyan.png" img
--
-- @since 0.3.3
lightCyan :: Color (SRGB 'NonLinear) Word8
lightCyan = ColorSRGB 224 255 255

-- | Defined in SVG1.1 as
--
-- @
-- lightgoldenrodyellow = rgb(250, 250, 210)
-- @
--
-- <<files/svg/LightGoldenRodYellow.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightGoldenRodYellow
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightGoldenRodYellow.png" img
--
-- @since 0.3.3
lightGoldenRodYellow :: Color (SRGB 'NonLinear) Word8
lightGoldenRodYellow = ColorSRGB 250 250 210

-- | Defined in SVG1.1 as
--
-- @
-- lightgray = rgb(211, 211, 211)
-- @
--
-- <<files/svg/LightGray.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightGray
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightGray.png" img
--
-- @since 0.3.3
lightGray :: Color (SRGB 'NonLinear) Word8
lightGray = ColorSRGB 211 211 211

-- | Defined in SVG1.1 as
--
-- @
-- lightgreen = rgb(144, 238, 144)
-- @
--
-- <<files/svg/LightGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightGreen.png" img
--
-- @since 0.3.3
lightGreen :: Color (SRGB 'NonLinear) Word8
lightGreen = ColorSRGB 144 238 144

-- | Defined in SVG1.1 as
--
-- @
-- lightgrey = rgb(211, 211, 211)
-- @
--
-- <<files/svg/LightGrey.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightGrey
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightGrey.png" img
--
-- @since 0.3.3
lightGrey :: Color (SRGB 'NonLinear) Word8
lightGrey = ColorSRGB 211 211 211

-- | Defined in SVG1.1 as
--
-- @
-- lightpink = rgb(255, 182, 193)
-- @
--
-- <<files/svg/LightPink.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightPink
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightPink.png" img
--
-- @since 0.3.3
lightPink :: Color (SRGB 'NonLinear) Word8
lightPink = ColorSRGB 255 182 193

-- | Defined in SVG1.1 as
--
-- @
-- lightsalmon = rgb(255, 160, 122)
-- @
--
-- <<files/svg/LightSalmon.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightSalmon
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightSalmon.png" img
--
-- @since 0.3.3
lightSalmon :: Color (SRGB 'NonLinear) Word8
lightSalmon = ColorSRGB 255 160 122

-- | Defined in SVG1.1 as
--
-- @
-- lightseagreen = rgb(32, 178, 170)
-- @
--
-- <<files/svg/LightSeaGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightSeaGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightSeaGreen.png" img
--
-- @since 0.3.3
lightSeaGreen :: Color (SRGB 'NonLinear) Word8
lightSeaGreen = ColorSRGB 32 178 170

-- | Defined in SVG1.1 as
--
-- @
-- lightskyblue = rgb(135, 206, 250)
-- @
--
-- <<files/svg/LightSkyBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightSkyBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightSkyBlue.png" img
--
-- @since 0.3.3
lightSkyBlue :: Color (SRGB 'NonLinear) Word8
lightSkyBlue = ColorSRGB 135 206 250

-- | Defined in SVG1.1 as
--
-- @
-- lightslategray = rgb(119, 136, 153)
-- @
--
-- <<files/svg/LightSlateGray.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightSlateGray
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightSlateGray.png" img
--
-- @since 0.3.3
lightSlateGray :: Color (SRGB 'NonLinear) Word8
lightSlateGray = ColorSRGB 119 136 153

-- | Defined in SVG1.1 as
--
-- @
-- lightslategrey = rgb(119, 136, 153)
-- @
--
-- <<files/svg/LightSlateGrey.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightSlateGrey
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightSlateGrey.png" img
--
-- @since 0.3.3
lightSlateGrey :: Color (SRGB 'NonLinear) Word8
lightSlateGrey = ColorSRGB 119 136 153

-- | Defined in SVG1.1 as
--
-- @
-- lightsteelblue = rgb(176, 196, 222)
-- @
--
-- <<files/svg/LightSteelBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightSteelBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightSteelBlue.png" img
--
-- @since 0.3.3
lightSteelBlue :: Color (SRGB 'NonLinear) Word8
lightSteelBlue = ColorSRGB 176 196 222

-- | Defined in SVG1.1 as
--
-- @
-- lightyellow = rgb(255, 255, 224)
-- @
--
-- <<files/svg/LightYellow.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lightYellow
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LightYellow.png" img
--
-- @since 0.3.3
lightYellow :: Color (SRGB 'NonLinear) Word8
lightYellow = ColorSRGB 255 255 224

-- | Defined in SVG1.1 as
--
-- @
-- lime = rgb(0, 255, 0)
-- @
--
-- <<files/svg/Lime.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = lime
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Lime.png" img
--
-- @since 0.3.3
lime :: Color (SRGB 'NonLinear) Word8
lime = ColorSRGB 0 255 0

-- | Defined in SVG1.1 as
--
-- @
-- limegreen = rgb(50, 205, 50)
-- @
--
-- <<files/svg/LimeGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = limeGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/LimeGreen.png" img
--
-- @since 0.3.3
limeGreen :: Color (SRGB 'NonLinear) Word8
limeGreen = ColorSRGB 50 205 50

-- | Defined in SVG1.1 as
--
-- @
-- linen = rgb(250, 240, 230)
-- @
--
-- <<files/svg/Linen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = linen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Linen.png" img
--
-- @since 0.3.3
linen :: Color (SRGB 'NonLinear) Word8
linen = ColorSRGB 250 240 230

-- | Defined in SVG1.1 as
--
-- @
-- magenta = rgb(255, 0, 255)
-- @
--
-- <<files/svg/Magenta.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = magenta
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Magenta.png" img
--
-- @since 0.3.3
magenta :: Color (SRGB 'NonLinear) Word8
magenta = ColorSRGB 255 0 255

-- | Defined in SVG1.1 as
--
-- @
-- maroon = rgb(128, 0, 0)
-- @
--
-- <<files/svg/Maroon.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = maroon
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Maroon.png" img
--
-- @since 0.3.3
maroon :: Color (SRGB 'NonLinear) Word8
maroon = ColorSRGB 128 0 0

-- | Defined in SVG1.1 as
--
-- @
-- mediumaquamarine = rgb(102, 205, 170)
-- @
--
-- <<files/svg/MediumAquaMarine.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumAquaMarine
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumAquaMarine.png" img
--
-- @since 0.3.3
mediumAquaMarine :: Color (SRGB 'NonLinear) Word8
mediumAquaMarine = ColorSRGB 102 205 170

-- | Defined in SVG1.1 as
--
-- @
-- mediumblue = rgb(0, 0, 205)
-- @
--
-- <<files/svg/MediumBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumBlue.png" img
--
-- @since 0.3.3
mediumBlue :: Color (SRGB 'NonLinear) Word8
mediumBlue = ColorSRGB 0 0 205

-- | Defined in SVG1.1 as
--
-- @
-- mediumorchid = rgb(186, 85, 211)
-- @
--
-- <<files/svg/MediumOrchid.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumOrchid
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumOrchid.png" img
--
-- @since 0.3.3
mediumOrchid :: Color (SRGB 'NonLinear) Word8
mediumOrchid = ColorSRGB 186 85 211

-- | Defined in SVG1.1 as
--
-- @
-- mediumpurple = rgb(147, 112, 219)
-- @
--
-- <<files/svg/MediumPurple.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumPurple
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumPurple.png" img
--
-- @since 0.3.3
mediumPurple :: Color (SRGB 'NonLinear) Word8
mediumPurple = ColorSRGB 147 112 219

-- | Defined in SVG1.1 as
--
-- @
-- mediumseagreen = rgb(60, 179, 113)
-- @
--
-- <<files/svg/MediumSeaGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumSeaGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumSeaGreen.png" img
--
-- @since 0.3.3
mediumSeaGreen :: Color (SRGB 'NonLinear) Word8
mediumSeaGreen = ColorSRGB 60 179 113

-- | Defined in SVG1.1 as
--
-- @
-- mediumslateblue = rgb(123, 104, 238)
-- @
--
-- <<files/svg/MediumSlateBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumSlateBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumSlateBlue.png" img
--
-- @since 0.3.3
mediumSlateBlue :: Color (SRGB 'NonLinear) Word8
mediumSlateBlue = ColorSRGB 123 104 238

-- | Defined in SVG1.1 as
--
-- @
-- mediumspringgreen = rgb(0, 250, 154)
-- @
--
-- <<files/svg/MediumSpringGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumSpringGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumSpringGreen.png" img
--
-- @since 0.3.3
mediumSpringGreen :: Color (SRGB 'NonLinear) Word8
mediumSpringGreen = ColorSRGB 0 250 154

-- | Defined in SVG1.1 as
--
-- @
-- mediumturquoise = rgb(72, 209, 204)
-- @
--
-- <<files/svg/MediumTurquoise.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumTurquoise
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumTurquoise.png" img
--
-- @since 0.3.3
mediumTurquoise :: Color (SRGB 'NonLinear) Word8
mediumTurquoise = ColorSRGB 72 209 204

-- | Defined in SVG1.1 as
--
-- @
-- mediumvioletred = rgb(199, 21, 133)
-- @
--
-- <<files/svg/MediumVioletRed.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mediumVioletRed
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MediumVioletRed.png" img
--
-- @since 0.3.3
mediumVioletRed :: Color (SRGB 'NonLinear) Word8
mediumVioletRed = ColorSRGB 199 21 133

-- | Defined in SVG1.1 as
--
-- @
-- midnightblue = rgb(25, 25, 112)
-- @
--
-- <<files/svg/MidnightBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = midnightBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MidnightBlue.png" img
--
-- @since 0.3.3
midnightBlue :: Color (SRGB 'NonLinear) Word8
midnightBlue = ColorSRGB 25 25 112

-- | Defined in SVG1.1 as
--
-- @
-- mintcream = rgb(245, 255, 250)
-- @
--
-- <<files/svg/MintCream.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mintCream
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MintCream.png" img
--
-- @since 0.3.3
mintCream :: Color (SRGB 'NonLinear) Word8
mintCream = ColorSRGB 245 255 250

-- | Defined in SVG1.1 as
--
-- @
-- mistyrose = rgb(255, 228, 225)
-- @
--
-- <<files/svg/MistyRose.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = mistyRose
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/MistyRose.png" img
--
-- @since 0.3.3
mistyRose :: Color (SRGB 'NonLinear) Word8
mistyRose = ColorSRGB 255 228 225

-- | Defined in SVG1.1 as
--
-- @
-- moccasin = rgb(255, 228, 181)
-- @
--
-- <<files/svg/Moccasin.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = moccasin
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Moccasin.png" img
--
-- @since 0.3.3
moccasin :: Color (SRGB 'NonLinear) Word8
moccasin = ColorSRGB 255 228 181

-- | Defined in SVG1.1 as
--
-- @
-- navajowhite = rgb(255, 222, 173)
-- @
--
-- <<files/svg/NavajoWhite.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = navajoWhite
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/NavajoWhite.png" img
--
-- @since 0.3.3
navajoWhite :: Color (SRGB 'NonLinear) Word8
navajoWhite = ColorSRGB 255 222 173

-- | Defined in SVG1.1 as
--
-- @
-- navy = rgb(0, 0, 128)
-- @
--
-- <<files/svg/Navy.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = navy
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Navy.png" img
--
-- @since 0.3.3
navy :: Color (SRGB 'NonLinear) Word8
navy = ColorSRGB 0 0 128

-- | Defined in SVG1.1 as
--
-- @
-- oldlace = rgb(253, 245, 230)
-- @
--
-- <<files/svg/OldLace.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = oldLace
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/OldLace.png" img
--
-- @since 0.3.3
oldLace :: Color (SRGB 'NonLinear) Word8
oldLace = ColorSRGB 253 245 230

-- | Defined in SVG1.1 as
--
-- @
-- olive = rgb(128, 128, 0)
-- @
--
-- <<files/svg/Olive.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = olive
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Olive.png" img
--
-- @since 0.3.3
olive :: Color (SRGB 'NonLinear) Word8
olive = ColorSRGB 128 128 0

-- | Defined in SVG1.1 as
--
-- @
-- olivedrab = rgb(107, 142, 35)
-- @
--
-- <<files/svg/OliveDrab.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = oliveDrab
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/OliveDrab.png" img
--
-- @since 0.3.3
oliveDrab :: Color (SRGB 'NonLinear) Word8
oliveDrab = ColorSRGB 107 142 35

-- | Defined in SVG1.1 as
--
-- @
-- orange = rgb(255, 165, 0)
-- @
--
-- <<files/svg/Orange.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = orange
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Orange.png" img
--
-- @since 0.3.3
orange :: Color (SRGB 'NonLinear) Word8
orange = ColorSRGB 255 165 0

-- | Defined in SVG1.1 as
--
-- @
-- orangered = rgb(255, 69, 0)
-- @
--
-- <<files/svg/OrangeRed.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = orangeRed
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/OrangeRed.png" img
--
-- @since 0.3.3
orangeRed :: Color (SRGB 'NonLinear) Word8
orangeRed = ColorSRGB 255 69 0

-- | Defined in SVG1.1 as
--
-- @
-- orchid = rgb(218, 112, 214)
-- @
--
-- <<files/svg/Orchid.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = orchid
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Orchid.png" img
--
-- @since 0.3.3
orchid :: Color (SRGB 'NonLinear) Word8
orchid = ColorSRGB 218 112 214

-- | Defined in SVG1.1 as
--
-- @
-- palegoldenrod = rgb(238, 232, 170)
-- @
--
-- <<files/svg/PaleGoldenRod.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = paleGoldenRod
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/PaleGoldenRod.png" img
--
-- @since 0.3.3
paleGoldenRod :: Color (SRGB 'NonLinear) Word8
paleGoldenRod = ColorSRGB 238 232 170

-- | Defined in SVG1.1 as
--
-- @
-- palegreen = rgb(152, 251, 152)
-- @
--
-- <<files/svg/PaleGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = paleGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/PaleGreen.png" img
--
-- @since 0.3.3
paleGreen :: Color (SRGB 'NonLinear) Word8
paleGreen = ColorSRGB 152 251 152

-- | Defined in SVG1.1 as
--
-- @
-- paleturquoise = rgb(175, 238, 238)
-- @
--
-- <<files/svg/PaleTurquoise.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = paleTurquoise
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/PaleTurquoise.png" img
--
-- @since 0.3.3
paleTurquoise :: Color (SRGB 'NonLinear) Word8
paleTurquoise = ColorSRGB 175 238 238

-- | Defined in SVG1.1 as
--
-- @
-- palevioletred = rgb(219, 112, 147)
-- @
--
-- <<files/svg/PaleVioletRed.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = paleVioletRed
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/PaleVioletRed.png" img
--
-- @since 0.3.3
paleVioletRed :: Color (SRGB 'NonLinear) Word8
paleVioletRed = ColorSRGB 219 112 147

-- | Defined in SVG1.1 as
--
-- @
-- papayawhip = rgb(255, 239, 213)
-- @
--
-- <<files/svg/PapayaWhip.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = papayaWhip
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/PapayaWhip.png" img
--
-- @since 0.3.3
papayaWhip :: Color (SRGB 'NonLinear) Word8
papayaWhip = ColorSRGB 255 239 213

-- | Defined in SVG1.1 as
--
-- @
-- peachpuff = rgb(255, 218, 185)
-- @
--
-- <<files/svg/PeachPuff.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = peachPuff
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/PeachPuff.png" img
--
-- @since 0.3.3
peachPuff :: Color (SRGB 'NonLinear) Word8
peachPuff = ColorSRGB 255 218 185

-- | Defined in SVG1.1 as
--
-- @
-- peru = rgb(205, 133, 63)
-- @
--
-- <<files/svg/Peru.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = peru
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Peru.png" img
--
-- @since 0.3.3
peru :: Color (SRGB 'NonLinear) Word8
peru = ColorSRGB 205 133 63

-- | Defined in SVG1.1 as
--
-- @
-- pink = rgb(255, 192, 203)
-- @
--
-- <<files/svg/Pink.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = pink
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Pink.png" img
--
-- @since 0.3.3
pink :: Color (SRGB 'NonLinear) Word8
pink = ColorSRGB 255 192 203

-- | Defined in SVG1.1 as
--
-- @
-- plum = rgb(221, 160, 221)
-- @
--
-- <<files/svg/Plum.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = plum
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Plum.png" img
--
-- @since 0.3.3
plum :: Color (SRGB 'NonLinear) Word8
plum = ColorSRGB 221 160 221

-- | Defined in SVG1.1 as
--
-- @
-- powderblue = rgb(176, 224, 230)
-- @
--
-- <<files/svg/PowderBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = powderBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/PowderBlue.png" img
--
-- @since 0.3.3
powderBlue :: Color (SRGB 'NonLinear) Word8
powderBlue = ColorSRGB 176 224 230

-- | Defined in SVG1.1 as
--
-- @
-- purple = rgb(128, 0, 128)
-- @
--
-- <<files/svg/Purple.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = purple
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Purple.png" img
--
-- @since 0.3.3
purple :: Color (SRGB 'NonLinear) Word8
purple = ColorSRGB 128 0 128

-- | Defined in SVG1.1 as
--
-- @
-- red = rgb(255, 0, 0)
-- @
--
-- <<files/svg/Red.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = red
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Red.png" img
--
-- @since 0.3.3
red :: Color (SRGB 'NonLinear) Word8
red = ColorSRGB 255 0 0

-- | Defined in SVG1.1 as
--
-- @
-- rosybrown = rgb(188, 143, 143)
-- @
--
-- <<files/svg/RosyBrown.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = rosyBrown
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/RosyBrown.png" img
--
-- @since 0.3.3
rosyBrown :: Color (SRGB 'NonLinear) Word8
rosyBrown = ColorSRGB 188 143 143

-- | Defined in SVG1.1 as
--
-- @
-- royalblue = rgb(65, 105, 225)
-- @
--
-- <<files/svg/RoyalBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = royalBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/RoyalBlue.png" img
--
-- @since 0.3.3
royalBlue :: Color (SRGB 'NonLinear) Word8
royalBlue = ColorSRGB 65 105 225

-- | Defined in SVG1.1 as
--
-- @
-- saddlebrown = rgb(139, 69, 19)
-- @
--
-- <<files/svg/SaddleBrown.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = saddleBrown
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SaddleBrown.png" img
--
-- @since 0.3.3
saddleBrown :: Color (SRGB 'NonLinear) Word8
saddleBrown = ColorSRGB 139 69 19

-- | Defined in SVG1.1 as
--
-- @
-- salmon = rgb(250, 128, 114)
-- @
--
-- <<files/svg/Salmon.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = salmon
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Salmon.png" img
--
-- @since 0.3.3
salmon :: Color (SRGB 'NonLinear) Word8
salmon = ColorSRGB 250 128 114

-- | Defined in SVG1.1 as
--
-- @
-- sandybrown = rgb(244, 164, 96)
-- @
--
-- <<files/svg/SandyBrown.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = sandyBrown
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SandyBrown.png" img
--
-- @since 0.3.3
sandyBrown :: Color (SRGB 'NonLinear) Word8
sandyBrown = ColorSRGB 244 164 96

-- | Defined in SVG1.1 as
--
-- @
-- seagreen = rgb(46, 139, 87)
-- @
--
-- <<files/svg/SeaGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = seaGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SeaGreen.png" img
--
-- @since 0.3.3
seaGreen :: Color (SRGB 'NonLinear) Word8
seaGreen = ColorSRGB 46 139 87

-- | Defined in SVG1.1 as
--
-- @
-- seashell = rgb(255, 245, 238)
-- @
--
-- <<files/svg/Seashell.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = seashell
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Seashell.png" img
--
-- @since 0.3.3
seashell :: Color (SRGB 'NonLinear) Word8
seashell = ColorSRGB 255 245 238

-- | Defined in SVG1.1 as
--
-- @
-- sienna = rgb(160, 82, 45)
-- @
--
-- <<files/svg/Sienna.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = sienna
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Sienna.png" img
--
-- @since 0.3.3
sienna :: Color (SRGB 'NonLinear) Word8
sienna = ColorSRGB 160 82 45

-- | Defined in SVG1.1 as
--
-- @
-- silver = rgb(192, 192, 192)
-- @
--
-- <<files/svg/Silver.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = silver
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Silver.png" img
--
-- @since 0.3.3
silver :: Color (SRGB 'NonLinear) Word8
silver = ColorSRGB 192 192 192

-- | Defined in SVG1.1 as
--
-- @
-- skyblue = rgb(135, 206, 235)
-- @
--
-- <<files/svg/SkyBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = skyBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SkyBlue.png" img
--
-- @since 0.3.3
skyBlue :: Color (SRGB 'NonLinear) Word8
skyBlue = ColorSRGB 135 206 235

-- | Defined in SVG1.1 as
--
-- @
-- slateblue = rgb(106, 90, 205)
-- @
--
-- <<files/svg/SlateBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = slateBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SlateBlue.png" img
--
-- @since 0.3.3
slateBlue :: Color (SRGB 'NonLinear) Word8
slateBlue = ColorSRGB 106 90 205

-- | Defined in SVG1.1 as
--
-- @
-- slategray = rgb(112, 128, 144)
-- @
--
-- <<files/svg/SlateGray.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = slateGray
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SlateGray.png" img
--
-- @since 0.3.3
slateGray :: Color (SRGB 'NonLinear) Word8
slateGray = ColorSRGB 112 128 144

-- | Defined in SVG1.1 as
--
-- @
-- slategrey = rgb(112, 128, 144)
-- @
--
-- <<files/svg/SlateGrey.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = slateGrey
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SlateGrey.png" img
--
-- @since 0.3.3
slateGrey :: Color (SRGB 'NonLinear) Word8
slateGrey = ColorSRGB 112 128 144

-- | Defined in SVG1.1 as
--
-- @
-- snow = rgb(255, 250, 250)
-- @
--
-- <<files/svg/Snow.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = snow
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Snow.png" img
--
-- @since 0.3.3
snow :: Color (SRGB 'NonLinear) Word8
snow = ColorSRGB 255 250 250

-- | Defined in SVG1.1 as
--
-- @
-- springgreen = rgb(0, 255, 127)
-- @
--
-- <<files/svg/SpringGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = springGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SpringGreen.png" img
--
-- @since 0.3.3
springGreen :: Color (SRGB 'NonLinear) Word8
springGreen = ColorSRGB 0 255 127

-- | Defined in SVG1.1 as
--
-- @
-- steelblue = rgb(70, 130, 180)
-- @
--
-- <<files/svg/SteelBlue.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = steelBlue
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/SteelBlue.png" img
--
-- @since 0.3.3
steelBlue :: Color (SRGB 'NonLinear) Word8
steelBlue = ColorSRGB 70 130 180

-- | Defined in SVG1.1 as
--
-- @
-- tan = rgb(210, 180, 140)
-- @
--
-- <<files/svg/Tan.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = tan
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Tan.png" img
--
-- @since 0.3.3
tan :: Color (SRGB 'NonLinear) Word8
tan = ColorSRGB 210 180 140

-- | Defined in SVG1.1 as
--
-- @
-- teal = rgb(0, 128, 128)
-- @
--
-- <<files/svg/Teal.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = teal
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Teal.png" img
--
-- @since 0.3.3
teal :: Color (SRGB 'NonLinear) Word8
teal = ColorSRGB 0 128 128

-- | Defined in SVG1.1 as
--
-- @
-- thistle = rgb(216, 191, 216)
-- @
--
-- <<files/svg/Thistle.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = thistle
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Thistle.png" img
--
-- @since 0.3.3
thistle :: Color (SRGB 'NonLinear) Word8
thistle = ColorSRGB 216 191 216

-- | Defined in SVG1.1 as
--
-- @
-- tomato = rgb(255, 99, 71)
-- @
--
-- <<files/svg/Tomato.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = tomato
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Tomato.png" img
--
-- @since 0.3.3
tomato :: Color (SRGB 'NonLinear) Word8
tomato = ColorSRGB 255 99 71

-- | Defined in SVG1.1 as
--
-- @
-- turquoise = rgb(64, 224, 208)
-- @
--
-- <<files/svg/Turquoise.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = turquoise
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Turquoise.png" img
--
-- @since 0.3.3
turquoise :: Color (SRGB 'NonLinear) Word8
turquoise = ColorSRGB 64 224 208

-- | Defined in SVG1.1 as
--
-- @
-- violet = rgb(238, 130, 238)
-- @
--
-- <<files/svg/Violet.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = violet
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Violet.png" img
--
-- @since 0.3.3
violet :: Color (SRGB 'NonLinear) Word8
violet = ColorSRGB 238 130 238

-- | Defined in SVG1.1 as
--
-- @
-- wheat = rgb(245, 222, 179)
-- @
--
-- <<files/svg/Wheat.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = wheat
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Wheat.png" img
--
-- @since 0.3.3
wheat :: Color (SRGB 'NonLinear) Word8
wheat = ColorSRGB 245 222 179

-- | Defined in SVG1.1 as
--
-- @
-- white = rgb(255, 255, 255)
-- @
--
-- <<files/svg/White.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = white
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/White.png" img
--
-- @since 0.3.3
white :: Color (SRGB 'NonLinear) Word8
white = ColorSRGB 255 255 255

-- | Defined in SVG1.1 as
--
-- @
-- whitesmoke = rgb(245, 245, 245)
-- @
--
-- <<files/svg/WhiteSmoke.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = whiteSmoke
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/WhiteSmoke.png" img
--
-- @since 0.3.3
whiteSmoke :: Color (SRGB 'NonLinear) Word8
whiteSmoke = ColorSRGB 245 245 245

-- | Defined in SVG1.1 as
--
-- @
-- yellow = rgb(255, 255, 0)
-- @
--
-- <<files/svg/Yellow.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = yellow
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/Yellow.png" img
--
-- @since 0.3.3
yellow :: Color (SRGB 'NonLinear) Word8
yellow = ColorSRGB 255 255 0

-- | Defined in SVG1.1 as
--
-- @
-- yellowgreen = rgb(154, 205, 50)
-- @
--
-- <<files/svg/YellowGreen.png>>
--
-- ===__Example__
--
-- >>> import Codec.Picture as JP
-- >>> import Codec.Picture.Png (writePng)
-- >>> let ColorSRGB r g b = yellowGreen
-- >>> let img = JP.generateImage (\_ _ -> JP.PixelRGB8 r g b) 200 29
-- >>> writePng "files/svg/YellowGreen.png" img
--
-- @since 0.3.3
yellowGreen :: Color (SRGB 'NonLinear) Word8
yellowGreen = ColorSRGB 154 205 50
