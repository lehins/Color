{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Graphics.ColorModel.Helpers
-- Copyright   : (c) Alexey Kuleshevich 2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Graphics.ColorModel.Helpers
  ( showsP
  , shows3
  , shows4
  , shows5
  , foldr3
  , foldr4
  , foldr5
  , traverse3
  , traverse4
  , traverse5
  , sizeOfN
  , alignmentN
  , peek3
  , poke3
  , peek4
  , poke4
  , peek5
  , poke5
  ) where

import Foreign.Ptr
import Foreign.Storable
import Graphics.ColorModel.Internal

-- Show helpers

channelSeparator :: ShowS
channelSeparator = ('|':)

shows3 :: Show a => a -> a -> a -> ShowS
shows3 c0 c1 c2 = shows c0 . channelSeparator . shows c1 . channelSeparator . shows c2

shows4 :: Show a => a -> a -> a -> a -> ShowS
shows4 c0 c1 c2 c3 = shows c0 . channelSeparator . shows3 c1 c2 c3

shows5 :: Show a => a -> a -> a -> a -> a -> ShowS
shows5 c0 c1 c2 c3 c4 = shows c0 . channelSeparator . shows4 c1 c2 c3 c4

showsP :: String -> ShowS -> ShowS
showsP t i = ('<' :) . (t ++) . (":>(" ++) . i . (")" ++)

_showsGP ::
     String
  -- ^ Color space name
  -> String
  -- ^ Illuminant name
  -> ShowS
  -> ShowS
_showsGP t i x = ('<':) . (t ++) . ((':':i) ++) . (">(" ++) . x . (")" ++)


-- Foldable helpers

foldr3 :: (e -> a -> a) -> a -> e -> e -> e -> a
foldr3 f acc c0 c1 c2 = f c0 (f c1 (f c2 acc))
{-# INLINE foldr3 #-}

foldr4 :: (e -> a -> a) -> a -> e -> e -> e -> e -> a
foldr4 f acc c0 c1 c2 c3 = f c0 (f c1 (f c2 (f c3 acc)))
{-# INLINE foldr4 #-}

foldr5 :: (e -> a -> a) -> a -> e -> e -> e -> e -> e -> a
foldr5 f acc c0 c1 c2 c3 c4 = f c0 (f c1 (f c2 (f c3 (f c4 acc))))
{-# INLINE foldr5 #-}

traverse3 :: Applicative f => (a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> f b
traverse3 g f c0 c1 c2 = g <$> f c0 <*> f c1 <*> f c2
{-# INLINE traverse3 #-}

traverse4 :: Applicative f => (a -> a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> t -> f b
traverse4 g f c0 c1 c2 c3 = g <$> f c0 <*> f c1 <*> f c2 <*> f c3
{-# INLINE traverse4 #-}

traverse5 ::
     Applicative f => (a -> a -> a -> a -> a -> b) -> (t -> f a) -> t -> t -> t -> t -> t -> f b
traverse5 g f c0 c1 c2 c3 c4 = g <$> f c0 <*> f c1 <*> f c2 <*> f c3 <*> f c4
{-# INLINE traverse5 #-}


-- Storable helpers

sizeOfN :: forall cs e . Storable e => Int -> Pixel cs e -> Int
sizeOfN n _ = n * sizeOf (undefined :: e)
{-# INLINE sizeOfN #-}

alignmentN :: forall cs e . Storable e => Int -> Pixel cs e -> Int
alignmentN _ _ = alignment (undefined :: e)
{-# INLINE alignmentN #-}

peek3 :: Storable e => (e -> e -> e -> Pixel cs e) -> Ptr (Pixel cs e) -> IO (Pixel cs e)
peek3 f p = do
  let q = castPtr p
  c0 <- peek q
  c1 <- peekElemOff q 1
  c2 <- peekElemOff q 2
  return $! f c0 c1 c2
{-# INLINE peek3 #-}

poke3 :: Storable e => Ptr (Pixel cs e) -> e -> e -> e -> IO ()
poke3 p c0 c1 c2 = do
  let q = castPtr p
  poke q c0
  pokeElemOff q 1 c1
  pokeElemOff q 2 c2
{-# INLINE poke3 #-}

peek4 ::
     forall cs e. Storable e
  => (e -> e -> e -> e -> Pixel cs e)
  -> Ptr (Pixel cs e)
  -> IO (Pixel cs e)
peek4 f p = do
  c0 <- peek (castPtr p)
  peek3 (f c0) (p `plusPtr` sizeOf (undefined :: e))
{-# INLINE peek4 #-}

poke4 :: forall cs e . Storable e => Ptr (Pixel cs e) -> e -> e -> e -> e -> IO ()
poke4 p c0 c1 c2 c3 = do
  poke (castPtr p) c0
  poke3 (p `plusPtr` sizeOf (undefined :: e)) c1 c2 c3
{-# INLINE poke4 #-}

peek5 ::
     forall cs e. Storable e
  => (e -> e -> e -> e -> e -> Pixel cs e)
  -> Ptr (Pixel cs e)
  -> IO (Pixel cs e)
peek5 f p = do
  c0 <- peek (castPtr p)
  peek4 (f c0) (p `plusPtr` sizeOf (undefined :: e))
{-# INLINE peek5 #-}

poke5 :: forall cs e . Storable e => Ptr (Pixel cs e) -> e -> e -> e -> e -> e -> IO ()
poke5 p c0 c1 c2 c3 c4 = do
  poke (castPtr p) c0
  poke4 (p `plusPtr` sizeOf (undefined :: e)) c1 c2 c3 c4
{-# INLINE poke5 #-}
