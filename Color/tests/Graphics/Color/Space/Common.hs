{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Color.Space.Common
  ( module Graphics.Color.Space
  , module Graphics.Color.Model.Common
  , colorSpaceSpec
  , colorSpaceLenientSpec
  , prop_toFromColorXYZ
  , prop_toFromLenientColorXYZ
  , prop_toFromBaseSpace
  ) where

import Graphics.Color.Space
import Graphics.Color.Model.Common

instance (Elevator e, Random e) => Arbitrary (Color (Y i) e) where
  arbitrary = Y <$> arbitraryElevator

instance (Elevator e, Random e) => Arbitrary (Color (XYZ i) e) where
  arbitrary = ColorXYZ <$> arbitraryElevator <*> arbitraryElevator <*> arbitraryElevator


prop_toFromColorXYZ ::
     forall cs i e. (ColorSpace cs i e, RealFloat e)
  => Color cs e
  -> Property
prop_toFromColorXYZ c = c `epsilonEqColor` fromColorXYZ (toColorXYZ c :: Color (XYZ i) Double)


-- For RGB standards, that have matrices rounded to 4 digits after the decimal point
prop_toFromLenientColorXYZ ::
     forall cs i e. (ColorSpace cs i e, RealFloat e)
  => e
  -> Color cs e
  -> Property
prop_toFromLenientColorXYZ epsilon c =
  epsilonEqColorTol epsilon c (fromColorXYZ (toColorXYZ c :: Color (XYZ i) Double))


prop_toFromBaseSpace ::
     forall cs i e. (ColorSpace cs i e, ColorSpace (BaseSpace cs) i e, RealFloat e)
  => Color cs e
  -> Property
prop_toFromBaseSpace c = c `epsilonEqColor` fromBaseSpace (toBaseSpace c)

prop_toFromBaseModel ::
     forall cs i e. ColorSpace cs i e
  => Color cs e
  -> Property
prop_toFromBaseModel c = c === fromBaseModel (toBaseModel c)

colorSpaceCommonSpec ::
     forall cs i e.
     (Arbitrary (Color cs e), ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, RealFloat e)
  => Spec -> Spec
colorSpaceCommonSpec extra =
  describe "ColorSpace" $ do
    prop "luminance . toColorXYZ" $ \(c :: Color cs e) ->
      (luminance c :: Color (Y i) Float) `epsilonEqColor`
      luminance (toColorXYZ c :: Color (XYZ i) Float)
    prop "toFromBaseModel" $ prop_toFromBaseModel @cs @i @e
    prop "toFromBaseSpace" $ prop_toFromBaseSpace @cs @i @e
    extra

colorSpaceSpec ::
     forall cs i e.
     (Arbitrary (Color cs e), ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, RealFloat e)
  => Spec
colorSpaceSpec =
  colorSpaceCommonSpec @cs @i @e $
    prop "toFromColorXYZ" $ prop_toFromColorXYZ @cs @i @e

colorSpaceLenientSpec ::
     forall cs i e.
     (Arbitrary (Color cs e), ColorSpace (BaseSpace cs) i e, ColorSpace cs i e, RealFloat e)
  => e
  -> Spec
colorSpaceLenientSpec tol =
  let tolStr = "(lenient=" ++ show tol ++ ")"
  in colorSpaceCommonSpec @cs @i @e $
       prop ("toFromColorXYZ " ++ tolStr) $ prop_toFromLenientColorXYZ @cs @i @e tol
