{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.Color.Standard.RALSpec (spec) where

import Control.Monad (zipWithM_, unless, when)
import Data.Kind
import GHC.TypeLits
import Graphics.Color.Model.Common hiding ((.&.))
import Graphics.Color.Space
import Graphics.Color.Standard.RAL as RAL
import Data.Bits


spec :: Spec
spec = do
  describe "RAL" $ do
    let ralsLAB = [(n, c) | (n, c, _) <- ralColors] :: [(String, Color (LAB D50) Float)]
        ralColors' :: ColorSpace cs i e => [(String, Color cs e)]
        ralColors' = [(n, c) | (n, c, d) <- ralColors, not d]
        matchColor (ename, expected) (rname, received) =
          unless (received == expected) $
          expectationFailure $
          concat
            [ "Recieved color: "
            , rname
            , " did not match the one produced by matching function: '"
            , ename
            , "': "
            , show received
            , " /= "
            , show expected
            ]
        specMatchLists pendingNames expect expectedColors receivedColors = do
          it "Lists have the same length" $ expectSameLength expectedColors receivedColors
          let mkTest expected (rname, received) =
                it rname $ do
                  when (rname `elem` pendingNames) $ pendingWith "Some discrepancy"
                  expect received expected
          zipWithM_ mkTest expectedColors receivedColors
    it "Color ShowS" $ do
      show (RAL :: RAL "GreenBeige") `shouldBe` "RAL \"GreenBeige\""
      show (Just (RAL :: RAL "GreenBeige")) `shouldBe` "Just (RAL \"GreenBeige\")"
    it "Color code matches" $ matchListsWith matchColor ralsLAB (toColorRAL ralColorCodes)
    it "Color name matches" $ matchListsWith matchColor ralsLAB (toColorRAL ralColorNames)
    let (srgbs, _hsls, _cmyks) = unzip3 ralAlternatives
    describe "Matches sRGB" $
      specMatchLists ["luminousBrightOrange", "pastelGreen"] shouldBe srgbs ralColors'
    -- xdescribe "Matches CMYK" $ specMatchLists [] shouldBe cmyks ralColors'
    -- xdescribe "Matches HSL" $ specMatchLists [] shouldBe hsls ralColors'



data HList (as :: [Type]) where
  HNil :: HList '[]
  (:>) :: a -> HList as -> HList (a ': as)

infixr 5 :>

class FromColorsRAL (xs :: [Type]) where
  toColorRAL :: ColorSpace cs i e => HList xs -> [(String, Color cs e)]

instance FromColorsRAL '[] where
  toColorRAL _ = []

instance (KnownNat code, StandardColor RAL code, FromColorsRAL xs) =>
         FromColorsRAL (RAL (code :: Nat) ': xs) where
  toColorRAL (c :> cs) = (show c, color c) : toColorRAL cs

instance (KnownSymbol name, StandardColor RAL name, FromColorsRAL xs) =>
         FromColorsRAL (RAL (name :: Symbol) ': xs) where
  toColorRAL (c :> cs) = (show c, color c) : toColorRAL cs


ralColorNames ::
  HList
  '[ RAL "Green beige"
   , RAL "Beige"
   , RAL "Sand yellow"
   , RAL "Signal yellow"
   , RAL "Golden yellow"
   , RAL "Honey yellow"
   , RAL "Maize yellow"
   , RAL "Daffodil yellow"
   , RAL "Brown beige"
   , RAL "Lemon yellow"
   , RAL "Oyster white"
   , RAL "Ivory"
   , RAL "Light ivory"
   , RAL "Sulfur yellow"
   , RAL "Saffron yellow"
   , RAL "Zinc yellow"
   , RAL "Grey beige"
   , RAL "Olive yellow"
   , RAL "Rape yellow"
   , RAL "Traffic yellow"
   , RAL "Ochre yellow"
   , RAL "Luminous yellow"
   , RAL "Curry yellow"
   , RAL "Melon yellow"
   , RAL "Broom yellow"
   , RAL "Dahlia yellow"
   , RAL "Pastel yellow"
   , RAL "Pearl beige"
   , RAL "Pearl gold"
   , RAL "Sun yellow"
   , RAL "Yellow orange"
   , RAL "Red orange"
   , RAL "Vermilion"
   , RAL "Pastel orange"
   , RAL "Pure orange"
   , RAL "Luminous orange"
   , RAL "Luminous bright orange"
   , RAL "Bright red orange"
   , RAL "Traffic orange"
   , RAL "Signal orange"
   , RAL "Deep orange"
   , RAL "Salmon orange"
   , RAL "Pearl orange"
   , RAL "Flame red"
   , RAL "Signal red"
   , RAL "Carmine red"
   , RAL "Ruby red"
   , RAL "Purple red"
   , RAL "Wine red"
   , RAL "Black red"
   , RAL "Oxide red"
   , RAL "Brown red"
   , RAL "Beige red"
   , RAL "Tomato red"
   , RAL "Antique pink"
   , RAL "Light pink"
   , RAL "Coral red"
   , RAL "Rose"
   , RAL "Strawberry red"
   , RAL "Traffic red"
   , RAL "Salmon pink"
   , RAL "Luminous red"
   , RAL "Luminous bright red"
   , RAL "Raspberry red"
   , RAL "Pure red"
   , RAL "Orient red"
   , RAL "Pearl ruby red"
   , RAL "Pearl pink"
   , RAL "Red lilac"
   , RAL "Red violet"
   , RAL "Heather violet"
   , RAL "Claret violet"
   , RAL "Blue lilac"
   , RAL "Traffic purple"
   , RAL "Purple violet"
   , RAL "Signal violet"
   , RAL "Pastel violet"
   , RAL "Telemagenta"
   , RAL "Pearl violet"
   , RAL "Pearl blackberry"
   , RAL "Violet blue"
   , RAL "Green blue"
   , RAL "Ultramarine blue"
   , RAL "Sapphire blue"
   , RAL "Black blue"
   , RAL "Signal blue"
   , RAL "Brilliant blue"
   , RAL "Grey blue"
   , RAL "Azure blue"
   , RAL "Gentian blue"
   , RAL "Steel blue"
   , RAL "Light blue"
   , RAL "Cobalt blue"
   , RAL "Pigeon blue"
   , RAL "Sky blue"
   , RAL "Traffic blue"
   , RAL "Turquoise blue"
   , RAL "Capri blue"
   , RAL "Ocean blue"
   , RAL "Water blue"
   , RAL "Night blue"
   , RAL "Distant blue"
   , RAL "Pastel blue"
   , RAL "Pearl gentian blue"
   , RAL "Pearl night blue"
   , RAL "Patina green"
   , RAL "Emerald green"
   , RAL "Leaf green"
   , RAL "Olive green"
   , RAL "Blue green"
   , RAL "Moss green"
   , RAL "Grey olive"
   , RAL "Bottle green"
   , RAL "Brown green"
   , RAL "Fir green"
   , RAL "Grass green"
   , RAL "Reseda green"
   , RAL "Black green"
   , RAL "Reed green"
   , RAL "Yellow olive"
   , RAL "Black olive"
   , RAL "Turquoise green"
   , RAL "May green"
   , RAL "Yellow green"
   , RAL "Pastel green"
   , RAL "Chrome green"
   , RAL "Pale green"
   , RAL "Olive-drab"
   , RAL "Brown olive"
   , RAL "Traffic green"
   , RAL "Fern green"
   , RAL "Opal green"
   , RAL "Light green"
   , RAL "Pine green"
   , RAL "Mint green"
   , RAL "Signal green"
   , RAL "Mint turquoise"
   , RAL "Pastel turquoise"
   , RAL "Pearl green"
   , RAL "Pearl opal green"
   , RAL "Pure green"
   , RAL "Luminous green"
   , RAL "Squirrel grey"
   , RAL "Silver grey"
   , RAL "Olive grey"
   , RAL "Moss grey"
   , RAL "Signal grey"
   , RAL "Mouse grey"
   , RAL "Beige grey"
   , RAL "Khaki grey"
   , RAL "Green grey"
   , RAL "Tarpaulin grey"
   , RAL "Iron grey"
   , RAL "Basalt grey"
   , RAL "Brown grey"
   , RAL "NATO olive"
   , RAL "Slate grey"
   , RAL "Anthracite grey"
   , RAL "Black grey"
   , RAL "Umbra grey"
   , RAL "Concrete grey"
   , RAL "Graphite grey"
   , RAL "Granite grey"
   , RAL "Stone grey"
   , RAL "Blue grey"
   , RAL "Pebble grey"
   , RAL "Cement grey"
   , RAL "Yellow grey"
   , RAL "Light grey"
   , RAL "Platinum grey"
   , RAL "Dusty grey"
   , RAL "Agate grey"
   , RAL "Quartz grey"
   , RAL "Window grey"
   , RAL "Traffic grey A"
   , RAL "Traffic grey B"
   , RAL "Silk grey"
   , RAL "Telegrey 1"
   , RAL "Telegrey 2"
   , RAL "Telegrey 4"
   , RAL "Pearl mouse grey"
   , RAL "Green brown"
   , RAL "Ochre brown"
   , RAL "Signal brown"
   , RAL "Clay brown"
   , RAL "Copper brown"
   , RAL "Fawn brown"
   , RAL "Olive brown"
   , RAL "Nut brown"
   , RAL "Red brown"
   , RAL "Sepia brown"
   , RAL "Chestnut brown"
   , RAL "Mahogany brown"
   , RAL "Chocolate brown"
   , RAL "Grey brown"
   , RAL "Black brown"
   , RAL "Orange brown"
   , RAL "Beige brown"
   , RAL "Pale brown"
   , RAL "Terra brown"
   , RAL "Pearl copper"
   , RAL "Cream"
   , RAL "Grey white"
   , RAL "Signal white"
   , RAL "Signal black"
   , RAL "Jet black"
   , RAL "White aluminium"
   , RAL "Grey aluminium"
   , RAL "Pure white"
   , RAL "Graphite black"
   , RAL "Traffic white"
   , RAL "Traffic black"
   , RAL "Papyrus white"
   , RAL "Pearl light grey"
   , RAL "Pearl dark grey"
   ]
ralColorNames =
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  HNil


ralColorCodes ::
  HList
  '[ RAL 1000
   , RAL 1001
   , RAL 1002
   , RAL 1003
   , RAL 1004
   , RAL 1005
   , RAL 1006
   , RAL 1007
   , RAL 1011
   , RAL 1012
   , RAL 1013
   , RAL 1014
   , RAL 1015
   , RAL 1016
   , RAL 1017
   , RAL 1018
   , RAL 1019
   , RAL 1020
   , RAL 1021
   , RAL 1023
   , RAL 1024
   , RAL 1026
   , RAL 1027
   , RAL 1028
   , RAL 1032
   , RAL 1033
   , RAL 1034
   , RAL 1035
   , RAL 1036
   , RAL 1037
   , RAL 2000
   , RAL 2001
   , RAL 2002
   , RAL 2003
   , RAL 2004
   , RAL 2005
   , RAL 2007
   , RAL 2008
   , RAL 2009
   , RAL 2010
   , RAL 2011
   , RAL 2012
   , RAL 2013
   , RAL 3000
   , RAL 3001
   , RAL 3002
   , RAL 3003
   , RAL 3004
   , RAL 3005
   , RAL 3007
   , RAL 3009
   , RAL 3011
   , RAL 3012
   , RAL 3013
   , RAL 3014
   , RAL 3015
   , RAL 3016
   , RAL 3017
   , RAL 3018
   , RAL 3020
   , RAL 3022
   , RAL 3024
   , RAL 3026
   , RAL 3027
   , RAL 3028
   , RAL 3031
   , RAL 3032
   , RAL 3033
   , RAL 4001
   , RAL 4002
   , RAL 4003
   , RAL 4004
   , RAL 4005
   , RAL 4006
   , RAL 4007
   , RAL 4008
   , RAL 4009
   , RAL 4010
   , RAL 4011
   , RAL 4012
   , RAL 5000
   , RAL 5001
   , RAL 5002
   , RAL 5003
   , RAL 5004
   , RAL 5005
   , RAL 5007
   , RAL 5008
   , RAL 5009
   , RAL 5010
   , RAL 5011
   , RAL 5012
   , RAL 5013
   , RAL 5014
   , RAL 5015
   , RAL 5017
   , RAL 5018
   , RAL 5019
   , RAL 5020
   , RAL 5021
   , RAL 5022
   , RAL 5023
   , RAL 5024
   , RAL 5025
   , RAL 5026
   , RAL 6000
   , RAL 6001
   , RAL 6002
   , RAL 6003
   , RAL 6004
   , RAL 6005
   , RAL 6006
   , RAL 6007
   , RAL 6008
   , RAL 6009
   , RAL 6010
   , RAL 6011
   , RAL 6012
   , RAL 6013
   , RAL 6014
   , RAL 6015
   , RAL 6016
   , RAL 6017
   , RAL 6018
   , RAL 6019
   , RAL 6020
   , RAL 6021
   , RAL 6022
   , RAL 6022 -- Synonym: Brown olive
   , RAL 6024
   , RAL 6025
   , RAL 6026
   , RAL 6027
   , RAL 6028
   , RAL 6029
   , RAL 6032
   , RAL 6033
   , RAL 6034
   , RAL 6035
   , RAL 6036
   , RAL 6037
   , RAL 6038
   , RAL 7000
   , RAL 7001
   , RAL 7002
   , RAL 7003
   , RAL 7004
   , RAL 7005
   , RAL 7006
   , RAL 7008
   , RAL 7009
   , RAL 7010
   , RAL 7011
   , RAL 7012
   , RAL 7013
   , RAL 7013
   , RAL 7015
   , RAL 7016
   , RAL 7021
   , RAL 7022
   , RAL 7023
   , RAL 7024
   , RAL 7026
   , RAL 7030
   , RAL 7031
   , RAL 7032
   , RAL 7033
   , RAL 7034
   , RAL 7035
   , RAL 7036
   , RAL 7037
   , RAL 7038
   , RAL 7039
   , RAL 7040
   , RAL 7042
   , RAL 7043
   , RAL 7044
   , RAL 7045
   , RAL 7046
   , RAL 7047
   , RAL 7048
   , RAL 8000
   , RAL 8001
   , RAL 8002
   , RAL 8003
   , RAL 8004
   , RAL 8007
   , RAL 8008
   , RAL 8011
   , RAL 8012
   , RAL 8014
   , RAL 8015
   , RAL 8016
   , RAL 8017
   , RAL 8019
   , RAL 8022
   , RAL 8023
   , RAL 8024
   , RAL 8025
   , RAL 8028
   , RAL 8029
   , RAL 9001
   , RAL 9002
   , RAL 9003
   , RAL 9004
   , RAL 9005
   , RAL 9006
   , RAL 9007
   , RAL 9010
   , RAL 9011
   , RAL 9016
   , RAL 9017
   , RAL 9018
   , RAL 9022
   , RAL 9023
   ]
ralColorCodes =
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  RAL :>
  HNil

-- Duplicates are marked by the boolean true.
ralColors :: ColorSpace cs i e => [(String, Color cs e, Bool)]
ralColors =
  [ ("greenBeige", greenBeige, False)
  , ("beige", beige, False)
  , ("sandYellow", sandYellow, False)
  , ("signalYellow", signalYellow, False)
  , ("goldenYellow", goldenYellow, False)
  , ("honeyYellow", honeyYellow, False)
  , ("maizeYellow", maizeYellow, False)
  , ("daffodilYellow", daffodilYellow, False)
  , ("brownBeige", brownBeige, False)
  , ("lemonYellow", lemonYellow, False)
  , ("oysterWhite", oysterWhite, False)
  , ("ivory", ivory, False)
  , ("lightIvory", lightIvory, False)
  , ("sulfurYellow", sulfurYellow, False)
  , ("saffronYellow", saffronYellow, False)
  , ("zincYellow", zincYellow, False)
  , ("greyBeige", greyBeige, False)
  , ("oliveYellow", oliveYellow, False)
  , ("rapeYellow", rapeYellow, False)
  , ("trafficYellow", trafficYellow, False)
  , ("ochreYellow", ochreYellow, False)
  , ("luminousYellow", luminousYellow, False)
  , ("curryYellow", curryYellow, False)
  , ("melonYellow", melonYellow, False)
  , ("broomYellow", broomYellow, False)
  , ("dahliaYellow", dahliaYellow, False)
  , ("pastelYellow", pastelYellow, False)
  , ("pearlBeige", pearlBeige, False)
  , ("pearlGold", pearlGold, False)
  , ("sunYellow", sunYellow, False)
  , ("yellowOrange", yellowOrange, False)
  , ("redOrange", redOrange, False)
  , ("vermilion", vermilion, False)
  , ("pastelOrange", pastelOrange, False)
  , ("pureOrange", pureOrange, False)
  , ("luminousOrange", luminousOrange, False)
  , ("luminousBrightOrange", luminousBrightOrange, False)
  , ("brightRedOrange", brightRedOrange, False)
  , ("trafficOrange", trafficOrange, False)
  , ("signalOrange", signalOrange, False)
  , ("deepOrange", deepOrange, False)
  , ("salmonOrange", salmonOrange, False)
  , ("pearlOrange", pearlOrange, False)
  , ("flameRed", flameRed, False)
  , ("signalRed", signalRed, False)
  , ("carmineRed", carmineRed, False)
  , ("rubyRed", rubyRed, False)
  , ("purpleRed", purpleRed, False)
  , ("wineRed", wineRed, False)
  , ("blackRed", blackRed, False)
  , ("oxideRed", oxideRed, False)
  , ("brownRed", brownRed, False)
  , ("beigeRed", beigeRed, False)
  , ("tomatoRed", tomatoRed, False)
  , ("antiquePink", antiquePink, False)
  , ("lightPink", lightPink, False)
  , ("coralRed", coralRed, False)
  , ("rose", rose, False)
  , ("strawberryRed", strawberryRed, False)
  , ("trafficRed", trafficRed, False)
  , ("salmonPink", salmonPink, False)
  , ("luminousRed", luminousRed, False)
  , ("luminousBrightRed", luminousBrightRed, False)
  , ("raspberryRed", raspberryRed, False)
  , ("pureRed", pureRed, False)
  , ("orientRed", orientRed, False)
  , ("pearlRubyRed", pearlRubyRed, False)
  , ("pearlPink", pearlPink, False)
  , ("redLilac", redLilac, False)
  , ("redViolet", redViolet, False)
  , ("heatherViolet", heatherViolet, False)
  , ("claretViolet", claretViolet, False)
  , ("blueLilac", blueLilac, False)
  , ("trafficPurple", trafficPurple, False)
  , ("purpleViolet", purpleViolet, False)
  , ("signalViolet", signalViolet, False)
  , ("pastelViolet", pastelViolet, False)
  , ("telemagenta", telemagenta, False)
  , ("pearlViolet", pearlViolet, False)
  , ("pearlBlackberry", pearlBlackberry, False)
  , ("violetBlue", violetBlue, False)
  , ("greenBlue", greenBlue, False)
  , ("ultramarineBlue", ultramarineBlue, False)
  , ("sapphireBlue", sapphireBlue, False)
  , ("blackBlue", blackBlue, False)
  , ("signalBlue", signalBlue, False)
  , ("brilliantBlue", brilliantBlue, False)
  , ("greyBlue", greyBlue, False)
  , ("azureBlue", azureBlue, False)
  , ("gentianBlue", gentianBlue, False)
  , ("steelBlue", steelBlue, False)
  , ("lightBlue", lightBlue, False)
  , ("cobaltBlue", cobaltBlue, False)
  , ("pigeonBlue", pigeonBlue, False)
  , ("skyBlue", skyBlue, False)
  , ("trafficBlue", trafficBlue, False)
  , ("turquoiseBlue", turquoiseBlue, False)
  , ("capriBlue", capriBlue, False)
  , ("oceanBlue", oceanBlue, False)
  , ("waterBlue", waterBlue, False)
  , ("nightBlue", nightBlue, False)
  , ("distantBlue", distantBlue, False)
  , ("pastelBlue", pastelBlue, False)
  , ("pearlGentianBlue", pearlGentianBlue, False)
  , ("pearlNightBlue", pearlNightBlue, False)
  , ("patinaGreen", patinaGreen, False)
  , ("emeraldGreen", emeraldGreen, False)
  , ("leafGreen", leafGreen, False)
  , ("oliveGreen", oliveGreen, False)
  , ("blueGreen", blueGreen, False)
  , ("mossGreen", mossGreen, False)
  , ("greyOlive", greyOlive, False)
  , ("bottleGreen", bottleGreen, False)
  , ("brownGreen", brownGreen, False)
  , ("firGreen", firGreen, False)
  , ("grassGreen", grassGreen, False)
  , ("resedaGreen", resedaGreen, False)
  , ("blackGreen", blackGreen, False)
  , ("reedGreen", reedGreen, False)
  , ("yellowOlive", yellowOlive, False)
  , ("blackOlive", blackOlive, False)
  , ("turquoiseGreen", turquoiseGreen, False)
  , ("mayGreen", mayGreen, False)
  , ("yellowGreen", yellowGreen, False)
  , ("pastelGreen", pastelGreen, False)
  , ("chromeGreen", chromeGreen, False)
  , ("paleGreen", paleGreen, False)
  , ("oliveDrab", oliveDrab, False)
  , ("brownOlive", oliveDrab, True)
  , ("trafficGreen", trafficGreen, False)
  , ("fernGreen", fernGreen, False)
  , ("opalGreen", opalGreen, False)
  , ("lightGreen", lightGreen, False)
  , ("pineGreen", pineGreen, False)
  , ("mintGreen", mintGreen, False)
  , ("signalGreen", signalGreen, False)
  , ("mintTurquoise", mintTurquoise, False)
  , ("pastelTurquoise", pastelTurquoise, False)
  , ("pearlGreen", pearlGreen, False)
  , ("pearlOpalGreen", pearlOpalGreen, False)
  , ("pureGreen", pureGreen, False)
  , ("luminousGreen", luminousGreen, False)
  , ("squirrelGrey", squirrelGrey, False)
  , ("silverGrey", silverGrey, False)
  , ("oliveGrey", oliveGrey, False)
  , ("mossGrey", mossGrey, False)
  , ("signalGrey", signalGrey, False)
  , ("mouseGrey", mouseGrey, False)
  , ("beigeGrey", beigeGrey, False)
  , ("khakiGrey", khakiGrey, False)
  , ("greenGrey", greenGrey, False)
  , ("tarpaulinGrey", tarpaulinGrey, False)
  , ("ironGrey", ironGrey, False)
  , ("basaltGrey", basaltGrey, False)
  , ("brownGrey", brownGrey, False)
  , ("brownGrey", brownGrey, True) -- Alt: NATO olive
  , ("slateGrey", slateGrey, False)
  , ("anthraciteGrey", anthraciteGrey, False)
  , ("blackGrey", blackGrey, False)
  , ("umbraGrey", umbraGrey, False)
  , ("concreteGrey", concreteGrey, False)
  , ("graphiteGrey", graphiteGrey, False)
  , ("graniteGrey", graniteGrey, False)
  , ("stoneGrey", stoneGrey, False)
  , ("blueGrey", blueGrey, False)
  , ("pebbleGrey", pebbleGrey, False)
  , ("cementGrey", cementGrey, False)
  , ("yellowGrey", yellowGrey, False)
  , ("lightGrey", lightGrey, False)
  , ("platinumGrey", platinumGrey, False)
  , ("dustyGrey", dustyGrey, False)
  , ("agateGrey", agateGrey, False)
  , ("quartzGrey", quartzGrey, False)
  , ("windowGrey", windowGrey, False)
  , ("trafficGreyA", trafficGreyA, False)
  , ("trafficGreyB", trafficGreyB, False)
  , ("silkGrey", silkGrey, False)
  , ("telegrey1", telegrey1, False)
  , ("telegrey2", telegrey2, False)
  , ("telegrey4", telegrey4, False)
  , ("pearlMouseGrey", pearlMouseGrey, False)
  , ("greenBrown", greenBrown, False)
  , ("ochreBrown", ochreBrown, False)
  , ("signalBrown", signalBrown, False)
  , ("clayBrown", clayBrown, False)
  , ("copperBrown", copperBrown, False)
  , ("fawnBrown", fawnBrown, False)
  , ("oliveBrown", oliveBrown, False)
  , ("nutBrown", nutBrown, False)
  , ("redBrown", redBrown, False)
  , ("sepiaBrown", sepiaBrown, False)
  , ("chestnutBrown", chestnutBrown, False)
  , ("mahoganyBrown", mahoganyBrown, False)
  , ("chocolateBrown", chocolateBrown, False)
  , ("greyBrown", greyBrown, False)
  , ("blackBrown", blackBrown, False)
  , ("orangeBrown", orangeBrown, False)
  , ("beigeBrown", beigeBrown, False)
  , ("paleBrown", paleBrown, False)
  , ("terraBrown", terraBrown, False)
  , ("pearlCopper", pearlCopper, False)
  , ("cream", cream, False)
  , ("greyWhite", greyWhite, False)
  , ("signalWhite", signalWhite, False)
  , ("signalBlack", signalBlack, False)
  , ("jetBlack", jetBlack, False)
  , ("whiteAluminium", whiteAluminium, False)
  , ("greyAluminium", greyAluminium, False)
  , ("pureWhite", pureWhite, False)
  , ("graphiteBlack", graphiteBlack, False)
  , ("trafficWhite", trafficWhite, False)
  , ("trafficBlack", trafficBlack, False)
  , ("papyrusWhite", papyrusWhite, False)
  , ("pearlLightGrey", pearlLightGrey, False)
  , ("pearlDarkGrey", pearlDarkGrey, False)
  ]


ralAlternatives ::
     [( Color (SRGB 'NonLinear) Word8
      , Color (HSL (SRGB 'NonLinear)) Float
      , Color (CMYK (SRGB 'NonLinear)) Word8)]
ralAlternatives =
  [ (ColorRGB r g b, ColorH360SL h (s / 100) (l / 100), ColorCMYK c m y k)
  | (_y', r, g, b, h, s, l, c, m, y, k) <- ralAlternativeRaw
  ]
  where
    ralAlternativeRaw ::
         [(Float, Word8, Word8, Word8, Float, Float, Float, Word8, Word8, Word8, Word8)]
    ralAlternativeRaw =
      [ (186.29, 201, 187, 136, 47.08, 37.57, 66.08, 0, 7, 32, 21)
      , (178.7, 204, 176, 131, 36.99, 41.71, 65.69, 0, 14, 36, 20)
      , (173.04, 205, 170, 109, 38.13, 48.98, 61.57, 0, 17, 47, 20)
      , (172.32, 242, 169, 0, 41.90, 100, 47.45, 0, 30, 100, 5)
      , (160.7, 221, 159, 0, 43.17, 100, 43.33, 0, 28, 100, 13)
      , (144.16, 197, 143, 0, 43.55, 100, 38.63, 0, 27, 100, 23)
      , (150.26, 219, 145, 0, 39.73, 100, 42.94, 0, 34, 100, 14)
      , (148.89, 226, 141, 0, 37.43, 100, 44.31, 0, 38, 100, 11)
      , (134.32, 171, 129, 79, 32.61, 36.8, 49.02, 0, 25, 54, 33)
      , (174.04, 214, 176, 37, 47.12, 70.52, 49.22, 0, 18, 83, 16)
      , (218.12, 225, 218, 199, 43.85, 30.23, 83.14, 0, 3, 12, 12)
      , (198.15, 217, 197, 154, 40.95, 45.32, 72.75, 0, 9, 29, 15)
      , (212.24, 227, 211, 181, 39.13, 45.1, 80, 0, 7, 20, 11)
      , (211.92, 232, 222, 53, 56.65, 79.56, 55.88, 0, 4, 77, 9)
      , (178.38, 240, 170, 80, 33.75, 84.21, 62.75, 0, 29, 67, 6)
      , (199.96, 242, 203, 46, 48.06, 88.29, 56.47, 0, 16, 81, 5)
      , (145.52, 162, 143, 122, 31.50, 17.7, 55.69, 0, 12, 25, 36)
      , (142.94, 157, 143, 101, 45, 22.22, 50.59, 0, 9, 36, 38)
      , (181.48, 238, 183, 0, 46.13, 100, 46.67, 0, 23, 100, 7)
      , (181.69, 239, 183, 0, 45.94, 100, 46.86, 0, 23, 100, 6)
      , (146.88, 181, 144, 75, 39.06, 41.73, 50.2, 0, 20, 59, 29)
      , (236.59, 255, 255, 0, 0.17, 100, 50, 0, 0, 100, 0)
      , (126.85, 162, 128, 12, 46.40, 86.21, 34.12, 0, 21, 93, 36)
      , (165.78, 255, 156, 0, 0.10, 100, 50, 0, 39, 100, 0)
      , (163.85, 219, 164, 0, 44.93, 100, 42.94, 0, 25, 100, 14)
      , (164.47, 243, 155, 27, 35.56, 90, 52.94, 0, 36, 89, 5)
      , (167.03, 230, 157, 81, 30.60, 74.87, 60.98, 0, 32, 65, 10)
      , (131.97, 142, 131, 112, 38, 11.81, 49.8, 0, 8, 21, 44)
      , (103.36, 125, 101, 63, 36.77, 32.98, 36.86, 0, 19, 50, 51)
      , (154.88, 234, 147, 0, 37.69, 100, 45.88, 0, 37, 100, 8)
      , (124.67, 213, 111, 0, 31.27, 100, 41.76, 0, 48, 100, 16)
      , (92.21, 182, 72, 28, 17.14, 73.33, 41.18, 0, 60, 85, 29)
      , (82.55, 188, 56, 35, 8.24, 68.61, 43.73, 0, 70, 81, 26)
      , (140.02, 241, 120, 41, 23.70, 87.72, 55.29, 0, 50, 83, 5)
      , (106.99, 222, 83, 6, 21.39, 94.74, 44.71, 0, 63, 97, 13)
      , (109.08, 255, 75, 17, 14.62, 100, 53.33, 0, 71, 93, 0)
      , (185.09, 255, 183, 0, 0.12, 100, 50, 0, 28, 100, 0)
      , (128.3, 232, 107, 34, 22.12, 81.15, 52.16, 0, 54, 85, 9)
      , (106.43, 218, 83, 10, 21.06, 91.23, 44.71, 0, 62, 95, 15)
      , (112.84, 204, 93, 41, 19.14, 66.53, 48.04, 0, 54, 80, 20)
      , (126.74, 221, 110, 15, 27.67, 87.29, 46.27, 0, 50, 93, 13)
      , (122.3, 209, 101, 78, 10.53, 58.74, 56.27, 0, 52, 63, 18)
      , (77.49, 143, 62, 38, 13.71, 58.01, 35.49, 0, 57, 73, 44)
      , (65.86, 164, 40, 33, 3.21, 66.5, 38.63, 0, 76, 80, 36)
      , (59.87, 152, 35, 35, 0, 62.57, 36.67, 0, 77, 77, 40)
      , (59.09, 152, 34, 34, 0, 63.44, 36.47, 0, 78, 78, 40)
      , (48.4, 132, 25, 34, 354.95, 68.15, 30.78, 0, 81, 74, 48)
      , (44.16, 105, 27, 35, 353.85, 59.09, 25.88, 0, 74, 67, 59)
      , (38.11, 88, 24, 31, 353.44, 57.14, 21.96, 0, 73, 65, 65)
      , (38.31, 61, 32, 34, 355.86, 31.18, 18.24, 0, 48, 44, 76)
      , (58.98, 102, 48, 41, 6.89, 42.66, 28.04, 0, 53, 60, 60)
      , (53.65, 119, 36, 36, 0, 53.55, 30.39, 0, 70, 70, 53)
      , (144.24, 194, 133, 109, 16.94, 41.06, 59.41, 0, 31, 44, 24)
      , (67.25, 149, 46, 37, 4.82, 60.22, 36.47, 0, 69, 75, 42)
      , (133.43, 201, 115, 117, 358.60, 44.33, 61.96, 0, 43, 42, 21)
      , (172.13, 215, 160, 166, 353.45, 40.74, 73.53, 0, 26, 23, 16)
      , (81.24, 164, 60, 48, 6.21, 54.72, 41.57, 0, 63, 71, 36)
      , (109.31, 200, 84, 93, 355.34, 51.33, 55.69, 0, 58, 53, 22)
      , (91.35, 196, 62, 74, 354.63, 53.17, 50.59, 0, 68, 62, 23)
      , (61.23, 184, 29, 19, 3.64, 81.28, 39.8, 0, 84, 90, 28)
      , (124.6, 204, 105, 85, 10.08, 53.85, 56.67, 0, 49, 58, 20)
      , (86.85, 255, 42, 36, 1.64, 100, 57.06, 0, 84, 86, 0)
      , (83.7, 255, 38, 32, 1.61, 100, 56.27, 0, 85, 87, 0)
      , (67.51, 169, 38, 61, 349.47, 63.29, 40.59, 0, 78, 64, 34)
      , (76.23, 201, 43, 38, 1.84, 68.2, 46.86, 0, 79, 81, 21)
      , (75.38, 164, 51, 56, 357.35, 52.56, 42.16, 0, 69, 66, 36)
      , (46.01, 110, 28, 36, 354.15, 59.42, 27.06, 0, 75, 67, 57)
      , (78.53, 162, 57, 46, 5.69, 55.77, 40.78, 0, 65, 72, 36)
      , (105.97, 131, 96, 131, 0.83, 15.42, 44.51, 0, 27, 0, 49)
      , (78.09, 140, 60, 75, 348.75, 40, 39.22, 0, 57, 46, 45)
      , (120.44, 196, 96, 140, 333.60, 45.87, 57.25, 0, 51, 29, 23)
      , (46.12, 100, 29, 57, 336.34, 55.04, 25.29, 0, 71, 43, 61)
      , (110.93, 123, 103, 154, 263.53, 20.16, 50.39, 20, 33, 0, 40)
      , (73.46, 145, 48, 115, 318.56, 50.26, 37.84, 0, 67, 21, 43)
      , (45.17, 71, 36, 60, 318.86, 32.71, 20.98, 0, 49, 15, 72)
      , (91.8, 135, 75, 131, 304, 28.57, 41.18, 0, 44, 3, 47)
      , (139.04, 157, 133, 146, 327.50, 10.91, 56.86, 0, 15, 7, 38)
      , (92.69, 187, 62, 119, 332.64, 50.2, 48.82, 0, 67, 36, 27)
      , (103.86, 113, 98, 135, 264.32, 15.88, 45.69, 16, 27, 0, 47)
      , (108.87, 109, 107, 127, 246, 8.55, 45.88, 14, 16, 0, 50)
      , (75.71, 56, 78, 111, 216, 32.93, 32.75, 50, 30, 0, 56)
      , (67.74, 29, 76, 100, 200.28, 55.04, 25.29, 71, 24, 0, 61)
      , (53.88, 30, 54, 123, 224.52, 60.78, 30, 76, 56, 0, 52)
      , (54.27, 38, 56, 85, 217.02, 38.21, 24.12, 55, 34, 0, 67)
      , (29.87, 26, 30, 40, 222.86, 21.21, 12.94, 35, 25, 0, 84)
      , (67.68, 0, 81, 135, 204, 100, 26.47, 100, 40, 0, 47)
      , (99.95, 66, 106, 140, 207.57, 35.92, 40.39, 53, 24, 0, 45)
      , (55.96, 45, 58, 68, 206.09, 20.35, 22.16, 34, 15, 0, 73)
      , (85.46, 45, 94, 120, 200.80, 45.45, 32.35, 62, 22, 0, 53)
      , (64.74, 0, 78, 124, 202.26, 100, 24.31, 100, 37, 0, 51)
      , (41.54, 30, 43, 61, 214.84, 34.07, 17.84, 51, 30, 0, 76)
      , (120.19, 46, 136, 182, 200.29, 59.65, 44.71, 75, 25, 0, 29)
      , (47.55, 34, 48, 83, 222.86, 41.88, 22.94, 59, 42, 0, 67)
      , (121.63, 104, 124, 150, 213.91, 18.11, 49.8, 31, 17, 0, 41)
      , (103.02, 11, 123, 176, 199.27, 88.24, 36.67, 94, 30, 0, 31)
      , (74.48, 0, 90, 140, 201.43, 100, 27.45, 100, 36, 0, 45)
      , (115.26, 27, 139, 140, 180.53, 67.66, 32.75, 81, 1, 0, 45)
      , (79.23, 15, 93, 132, 200, 79.59, 28.82, 89, 30, 0, 48)
      , (51.9, 0, 65, 75, 188, 100, 14.71, 100, 13, 0, 71)
      , (92.27, 0, 117, 119, 181.01, 100, 23.33, 100, 2, 0, 53)
      , (47.11, 43, 44, 90, 238.72, 35.34, 26.08, 52, 51, 0, 65)
      , (100.29, 74, 104, 141, 213.13, 31.16, 42.16, 48, 26, 0, 45)
      , (138.74, 103, 146, 172, 202.61, 29.36, 53.92, 40, 15, 0, 33)
      , (93.4, 44, 105, 124, 194.25, 47.62, 32.94, 65, 15, 0, 51)
      , (45.28, 27, 47, 82, 218.18, 50.46, 21.37, 67, 43, 0, 68)
      , (102.44, 59, 116, 96, 158.95, 32.57, 34.31, 49, 0, 17, 55)
      , (88.55, 49, 104, 52, 123.27, 35.95, 30, 53, 0, 50, 59)
      , (76.75, 45, 90, 39, 112.94, 39.53, 25.29, 50, 0, 57, 65)
      , (80.2, 78, 83, 59, 72.50, 16.9, 27.84, 6, 0, 29, 67)
      , (55.1, 8, 68, 66, 178, 78.95, 14.9, 88, 0, 3, 73)
      , (54.43, 17, 66, 50, 160.41, 59.04, 16.27, 74, 0, 24, 74)
      , (56.63, 59, 57, 46, 50.77, 12.38, 20.59, 0, 3, 22, 77)
      , (47.14, 42, 50, 34, 90, 19.05, 16.47, 16, 0, 32, 80)
      , (51.7, 54, 52, 42, 50, 12.5, 18.82, 0, 4, 22, 79)
      , (49.94, 39, 54, 42, 132, 16.13, 18.24, 28, 0, 22, 79)
      , (98.74, 72, 111, 56, 102.55, 32.93, 32.75, 35, 0, 50, 56)
      , (118.08, 105, 125, 88, 92.43, 17.37, 41.76, 16, 0, 30, 51)
      , (58.02, 48, 61, 58, 166.15, 11.93, 21.37, 21, 0, 5, 76)
      , (116.83, 122, 118, 90, 52.50, 15.09, 41.57, 0, 3, 26, 52)
      , (65.2, 70, 65, 53, 42.35, 13.82, 24.12, 0, 7, 24, 73)
      , (60.28, 60, 61, 54, 68.57, 6.09, 22.55, 2, 0, 11, 76)
      , (81.3, 0, 106, 76, 163.02, 100, 20.78, 100, 0, 28, 58)
      , (113.74, 83, 128, 63, 101.54, 34.03, 37.45, 35, 0, 51, 50)
      , (133.18, 89, 154, 57, 100.21, 45.97, 41.37, 42, 0, 63, 40)
      , (198.66, 183, 206, 172, 100.59, 25.76, 74.12, 11, 0, 17, 19)
      , (62.08, 54, 66, 47, 97.89, 16.81, 22.16, 18, 0, 29, 74)
      , (147.43, 135, 154, 119, 92.57, 14.77, 53.53, 12, 0, 23, 40)
      , (51.41, 57, 51, 39, 40, 18.75, 18.82, 0, 11, 32, 78)
      , (100.18, 0, 132, 80, 156.36, 100, 25.88, 100, 0, 39, 48)
      , (102.07, 90, 110, 59, 83.53, 30.18, 33.14, 18, 0, 46, 57)
      , (73.58, 0, 95, 78, 169.26, 100, 18.63, 100, 0, 18, 63)
      , (173.31, 128, 186, 181, 174.83, 29.59, 61.57, 31, 0, 3, 27)
      , (75.05, 48, 84, 66, 150, 27.27, 25.88, 43, 0, 21, 67)
      , (84.43, 0, 112, 60, 152.14, 100, 21.96, 100, 0, 46, 56)
      , (103.35, 28, 128, 81, 151.80, 64.1, 30.59, 78, 0, 37, 50)
      , (121.03, 72, 135, 127, 172.38, 30.43, 40.59, 47, 0, 6, 47)
      , (162.51, 124, 173, 172, 178.78, 23, 58.24, 28, 0, 1, 32)
      , (61.71, 19, 77, 36, 137.59, 60.42, 18.82, 75, 0, 53, 70)
      , (69.84, 7, 88, 75, 170.37, 85.26, 18.63, 92, 0, 15, 65)
      , (102.94, 0, 140, 39, 136.71, 100, 27.45, 100, 0, 72, 45)
      , (131.47, 0, 182, 18, 125.93, 100, 35.69, 100, 0, 90, 29)
      , (133.67, 123, 136, 142, 198.95, 7.76, 51.96, 13, 4, 0, 44)
      , (148.8, 142, 150, 157, 208, 7.11, 58.63, 10, 4, 0, 38)
      , (119.97, 127, 120, 99, 45, 12.39, 44.31, 0, 6, 22, 50)
      , (118.2, 120, 119, 105, 56, 6.67, 44.12, 0, 1, 13, 53)
      , (155, 155, 155, 155, 0.61, 0, 60.78, 0, 0, 0, 39)
      , (109.15, 107, 110, 107, 120, 1.38, 42.55, 3, 0, 3, 57)
      , (107.47, 117, 106, 94, 31.30, 10.9, 41.37, 0, 9, 20, 54)
      , (96.51, 114, 95, 60, 38.89, 31.03, 34.12, 0, 17, 47, 55)
      , (94.57, 92, 96, 88, 90, 4.35, 36.08, 4, 0, 8, 62)
      , (90.72, 88, 92, 86, 100, 3.37, 34.9, 4, 0, 7, 64)
      , (88.01, 83, 89, 93, 204, 5.68, 34.51, 11, 4, 0, 64)
      , (92.01, 88, 93, 94, 190, 3.3, 35.69, 6, 1, 0, 63)
      , (80.41, 86, 80, 68, 40, 11.69, 30.2, 0, 7, 21, 66)
      , (82.8, 80, 83, 89, 220, 5.33, 33.14, 10, 7, 0, 65)
      , (61.01, 56, 62, 66, 204, 8.2, 23.92, 15, 6, 0, 74)
      , (49.72, 48, 50, 52, 210, 4, 19.61, 8, 4, 0, 80)
      , (73.99, 76, 74, 68, 45, 5.56, 28.24, 0, 3, 11, 70)
      , (127.07, 127, 128, 118, 66, 4.07, 48.24, 1, 0, 8, 50)
      , (72.8, 70, 73, 79, 220, 6.04, 29.22, 11, 8, 0, 69)
      , (64.81, 56, 67, 69, 189.23, 10.4, 24.51, 19, 3, 0, 73)
      , (141.99, 145, 142, 133, 45, 5.17, 54.51, 0, 2, 8, 43)
      , (102.02, 93, 104, 109, 198.75, 7.92, 39.61, 15, 5, 0, 57)
      , (175.77, 180, 176, 161, 47.37, 11.24, 66.86, 0, 2, 11, 29)
      , (128.14, 126, 130, 116, 77.14, 5.69, 48.24, 3, 0, 11, 49)
      , (135.9, 144, 136, 111, 0.13, 12.94, 50, 0, 6, 23, 44)
      , (198.36, 197, 199, 196, 100, 2.61, 77.45, 1, 0, 2, 22)
      , (147.78, 151, 147, 146, 12, 2.35, 58.24, 0, 3, 3, 41)
      , (122.72, 122, 123, 122, 120, 0.41, 48.04, 1, 0, 1, 52)
      , (176, 175, 177, 169, 75, 4.88, 67.84, 1, 0, 5, 31)
      , (102.27, 106, 102, 94, 40, 6, 39.22, 0, 4, 11, 58)
      , (156.94, 152, 158, 161, 200, 4.57, 61.37, 6, 2, 0, 37)
      , (145.08, 142, 146, 145, 165, 1.8, 56.47, 3, 0, 1, 43)
      , (81.22, 79, 82, 80, 140, 1.86, 31.57, 4, 0, 2, 68)
      , (178.84, 182, 179, 168, 47.14, 8.75, 68.63, 0, 2, 8, 29)
      , (145.37, 142, 146, 149, 205.71, 3.2, 57.06, 5, 2, 0, 42)
      , (132.8, 127, 134, 138, 201.82, 4.49, 51.96, 8, 3, 0, 46)
      , (199.93, 200, 200, 199, 0.17, 0.9, 78.24, 0, 0, 0, 22)
      , (123.49, 128, 123, 115, 36.92, 5.35, 47.65, 0, 4, 10, 50)
      , (108.78, 134, 106, 62, 36.67, 36.73, 38.43, 0, 21, 54, 47)
      , (106.44, 153, 99, 43, 30.55, 56.12, 38.43, 0, 35, 72, 40)
      , (84.85, 119, 77, 62, 15.79, 31.49, 35.49, 0, 35, 48, 53)
      , (82.82, 124, 75, 39, 25.41, 52.15, 31.96, 0, 40, 69, 51)
      , (85.09, 138, 73, 49, 16.18, 47.59, 36.67, 0, 47, 64, 46)
      , (76.34, 109, 70, 43, 24.55, 43.42, 29.8, 0, 36, 61, 57)
      , (79.19, 111, 74, 37, 30, 50, 29.02, 0, 33, 67, 56)
      , (61.58, 88, 56, 39, 20.82, 38.58, 24.9, 0, 36, 56, 65)
      , (60.84, 100, 51, 43, 8.42, 39.86, 28.04, 0, 49, 57, 61)
      , (55.96, 72, 53, 38, 26.47, 30.91, 21.57, 0, 26, 47, 72)
      , (56.2, 93, 47, 39, 8.89, 40.91, 25.88, 0, 49, 58, 64)
      , (49.01, 75, 43, 32, 15.35, 40.19, 20.98, 0, 43, 57, 71)
      , (50.82, 67, 47, 41, 13.85, 24.07, 21.18, 0, 30, 39, 74)
      , (55.42, 61, 54, 53, 7.50, 7.02, 22.35, 0, 11, 13, 76)
      , (23.78, 26, 23, 25, 320, 6.12, 9.61, 0, 12, 4, 90)
      , (99.2, 160, 87, 41, 23.19, 59.2, 39.41, 0, 46, 74, 37)
      , (86.35, 118, 80, 56, 23.23, 35.63, 34.12, 0, 32, 53, 54)
      , (92.51, 115, 88, 71, 23.18, 23.66, 36.47, 0, 23, 38, 55)
      , (61.31, 79, 58, 42, 25.95, 30.58, 23.73, 0, 27, 47, 69)
      , (75.89, 125, 64, 49, 11.84, 43.68, 34.12, 0, 49, 61, 51)
      , (225.19, 231, 225, 210, 42.86, 30.43, 86.47, 0, 3, 9, 9)
      , (212.49, 214, 213, 203, 54.55, 11.83, 81.76, 0, 0, 5, 16)
      , (235.64, 236, 236, 231, 0.17, 11.63, 91.57, 0, 0, 2, 7)
      , (43.07, 43, 43, 44, 240, 1.15, 17.06, 2, 2, 0, 83)
      , (14.14, 14, 14, 16, 240, 6.67, 5.88, 12, 12, 0, 94)
      , (160.93, 161, 161, 160, 0.17, 0.53, 62.94, 0, 0, 1, 37)
      , (132.92, 134, 133, 129, 48, 2.02, 51.57, 0, 1, 4, 47)
      , (236.77, 240, 237, 225, 48, 33.33, 91.18, 0, 1, 6, 6)
      , (40.72, 39, 41, 43, 210, 4.88, 16.08, 9, 5, 0, 83)
      , (240.28, 240, 241, 234, 68.57, 20, 93.14, 0, 0, 3, 5)
      , (41.28, 42, 41, 42, 0.83, 1.2, 16.27, 0, 2, 0, 84)
      , (201.64, 199, 203, 196, 94.29, 6.31, 78.24, 2, 0, 3, 20)
      , (132.86, 133, 133, 131, 0.17, 0.81, 51.76, 0, 0, 2, 48)
      , (122.57, 121, 123, 123, 0.50, 0.82, 47.84, 2, 0, 0, 52)
      ]

_hexes :: [Color (SRGB 'NonLinear) Word8]
_hexes =
  [ fromIntegral <$>
  fromComponents (shiftR (x .&. 0xFF0000) 16, shiftR (x .&. 0xFF00) 8, x .&. 0xFF)
  | x <-
      [ 0xCDBA88 :: Word32
      , 0xD0B084
      , 0xD2AA6D
      , 0xF9A800
      , 0xE49E00
      , 0xCB8E00
      , 0xE29000
      , 0xE88C00
      , 0xAF804F
      , 0xDDAF27
      , 0xE3D9C6
      , 0xDDC49A
      , 0xE6D2B5
      , 0xF1DD38
      , 0xF6A950
      , 0xFACA30
      , 0xA48F7A
      , 0xA08F65
      , 0xF6B600
      , 0xF7B500
      , 0xBA8F4C
      , 0xFFFF00
      , 0xA77F0E
      , 0xFF9B00
      , 0xE2A300
      , 0xF99A1C
      , 0xEB9C52
      , 0x908370
      , 0x80643F
      , 0xF09200
      , 0xDD7907
      , 0xBE4E20
      , 0xC63927
      , 0xFA842B
      , 0xE75B12
      , 0xFF2300
      , 0xFFA421
      , 0xF3752C
      , 0xE15501
      , 0xD4652F
      , 0xEC7C25
      , 0xDB6A50
      , 0x954527
      , 0xAB2524
      , 0xA02128
      , 0xA1232B
      , 0x8D1D2C
      , 0x701F29
      , 0x5E2028
      , 0x402225
      , 0x703731
      , 0x7E292C
      , 0xCB8D73
      , 0x9C322E
      , 0xD47479
      , 0xE1A6AD
      , 0xAC4034
      , 0xD3545F
      , 0xD14152
      , 0xC1121C
      , 0xD56D56
      , 0xF70000
      , 0xFF0000
      , 0xB42041
      , 0xE72512
      , 0xAC323B
      , 0x711521
      , 0xB24C43
      , 0x8A5A83
      , 0x933D50
      , 0xD15B8F
      , 0x691639
      , 0x83639D
      , 0x992572
      , 0x4A203B
      , 0x904684
      , 0xA38995
      , 0xC63678
      , 0x8773A1
      , 0x6B6880
      , 0x384C70
      , 0x1F4764
      , 0x2B2C7C
      , 0x2A3756
      , 0x1D1F2A
      , 0x154889
      , 0x41678D
      , 0x313C48
      , 0x2E5978
      , 0x13447C
      , 0x232C3F
      , 0x3481B8
      , 0x232D53
      , 0x6C7C98
      , 0x2874B2
      , 0x0E518D
      , 0x21888F
      , 0x1A5784
      , 0x0B4151
      , 0x07737A
      , 0x2F2A5A
      , 0x4D668E
      , 0x6A93B0
      , 0x296478
      , 0x102C54
      , 0x327662
      , 0x28713E
      , 0x276235
      , 0x4B573E
      , 0x0E4243
      , 0x0F4336
      , 0x40433B
      , 0x283424
      , 0x35382E
      , 0x26392F
      , 0x3E753B
      , 0x68825B
      , 0x31403D
      , 0x797C5A
      , 0x444337
      , 0x3D403A
      , 0x026A52
      , 0x468641
      , 0x48A43F
      , 0xB7D9B1
      , 0x354733
      , 0x86A47C
      , 0x3E3C32
      , 0x008754
      , 0x53753C
      , 0x005D52
      , 0x81C0BB
      , 0x2D5546
      , 0x007243
      , 0x0F8558
      , 0x478A84
      , 0x7FB0B2
      , 0x1B542C
      , 0x005D4C
      , 0x25E712
      , 0x00F700
      , 0x7E8B92
      , 0x8F999F
      , 0x817F68
      , 0x7A7B6D
      , 0x9EA0A1
      , 0x6B716F
      , 0x756F61
      , 0x746643
      , 0x5B6259
      , 0x575D57
      , 0x555D61
      , 0x596163
      , 0x555548
      , 0x51565C
      , 0x373F43
      , 0x2E3234
      , 0x4B4D46
      , 0x818479
      , 0x474A50
      , 0x374447
      , 0x939388
      , 0x5D6970
      , 0xB9B9A8
      , 0x818979
      , 0x939176
      , 0xCBD0CC
      , 0x9A9697
      , 0x7C7F7E
      , 0xB4B8B0
      , 0x6B695F
      , 0x9DA3A6
      , 0x8F9695
      , 0x4E5451
      , 0xBDBDB2
      , 0x91969A
      , 0x82898E
      , 0xCFD0CF
      , 0x888175
      , 0x887142
      , 0x9C6B30
      , 0x7B5141
      , 0x80542F
      , 0x8F4E35
      , 0x6F4A2F
      , 0x6F4F28
      , 0x5A3A29
      , 0x673831
      , 0x49392D
      , 0x633A34
      , 0x4C2F26
      , 0x44322D
      , 0x3F3A3A
      , 0x211F20
      , 0xA65E2F
      , 0x79553C
      , 0x755C49
      , 0x4E3B2B
      , 0x773C27
      , 0xEFEBDC
      , 0xDDDED4
      , 0xF4F8F4
      , 0x2E3032
      , 0x0A0A0D
      , 0xA5A8A6
      , 0x8F8F8C
      , 0xF7F9EF
      , 0x292C2F
      , 0xF7FBF5
      , 0x2A2D2F
      , 0xCFD3CD
      , 0x9C9C9C
      , 0x7E8182
      ]
  ]
