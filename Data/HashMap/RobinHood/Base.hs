{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.HashMap.RobinHood.Base
  ( RH
  , new
  , remove
  , insert
  , Data.HashMap.RobinHood.Base.lookup
  , member
  , lookupIndex
  , toList
  , size
  , averageProbeCount
  , load
  ) where

import           Data.Bits                   ((.&.), (.|.))

import qualified Data.Hashable               as H

import Data.HashMap.RobinHood.Monad
import Data.HashMap.RobinHood.Ref


data RH m key value = RH { _mask            :: {-# UNPACK #-} !Mask
                         , _capacity        :: {-# UNPACK #-} !Int
                         , _resizeThreshold :: {-# UNPACK #-} !Int
                         , _elemCount       :: !(Ref m Int)
                         , _hashVector      :: !(UnboxedArray m Int)
                         , _elemVector      :: !(BoxedArray m (ELEM_kv key value))
                         }

-- data Elem key value = Elem {-# UNPACK #-} !Hash !key !value

class Elem_kv k v where
  data ELEM_kv k v
  mk :: Hash -> k -> v -> ELEM_kv k v
  gt :: ELEM_kv k v -> (Hash, k, v)

instance Elem_kv Int () where
  data ELEM_kv Int () = ELEM_Int {-# UNPACK #-} !Int !()
  mk _h k v = ELEM_Int k v
  gt (ELEM_Int k v) = (mkHash k, k, v)

instance Elem_kv Int Int where
  data ELEM_kv Int Int = ELEM_IntInt {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  mk _h k v = ELEM_IntInt k v
  gt (ELEM_IntInt k v) = (mkHash k, k, v)

newtype Mask = Mask { unMask :: Int } deriving (Eq, Show)
newtype Hash = Hash { unHash :: Int } deriving (Eq, Show)
newtype Pos = Pos { unPos :: Int } deriving (Eq, Show)

lOAD_FACTOR_PERCENT :: Int
lOAD_FACTOR_PERCENT = 90

new :: (WriterM m, Elem_kv key value) => m (RH m key value)
new = alloc 16

alloc :: (WriterM m) => Int -> m (RH m key value)
alloc !capacity = do
  h <- newUnboxedArray capacity (0::Int)
  e <- newBoxedArray capacity
  count <- newRef (0::Int)
  let mask = Mask (capacity - 1)
      resizeThreshold = capacity * lOAD_FACTOR_PERCENT `div` 100
  return (RH mask capacity resizeThreshold count h e)

grow :: (WriterM m, H.Hashable key, Elem_kv key value)
     => (RH m key value) -> m (RH m key value)
grow rh0 = do
  rhNew <- alloc (_capacity rh0 * 2)
  iter rh0 (\rh k v -> insert rh k v) rhNew

{-# INLINE insert #-}
insert :: (WriterM m, H.Hashable key, Elem_kv key value) => RH m key value -> key -> value -> m (RH m key value)
insert rh@(RH _ _ resizeThreshold elemCount _ _) key value = do
  cnt <- readRef elemCount
  if (cnt + 1 >= resizeThreshold)
    then do
      rh' <- grow rh
      insertHelper rh' key value
      return rh'
    else do
      insertHelper rh key value
      return rh

{-# INLINE insertHelper #-}
insertHelper :: (WriterM m, H.Hashable key, Elem_kv key value) => RH m key value -> key -> value -> m ()
insertHelper rh@(RH mask _ _ elemCount hV eV) key0 value0 = do
  go hash0 (mk hash0 key0 value0) pos0 0
  modifyRef elemCount (+(1::Int))
  where
    hash0 = mkHash key0
    pos0 = desiredPos mask hash0
    go !hash !e !pos !dist = do
      hash' <- readHash hV pos
      let existing_elem_probe_dist = probeDistance rh hash' pos
      case () of
        _ | hash' == Hash 0 -> put pos hash e
          | existing_elem_probe_dist < dist -> case () of
              _ | isRemovedHash hash' -> put pos hash e
                | otherwise -> do
                     eA <- readElem eV pos
                     let (h', _, _) = gt eA
                     -- e'@(Elem h' _ _) <- readElem eV pos
                     put pos hash e
                     go h' eA (incPos mask pos) (existing_elem_probe_dist + 1)
          | otherwise -> go hash e (incPos mask pos) (dist + 1)
    put !pos !hash !e = do
      writeHash hV pos hash
      writeElem eV pos e
      return ()

{-# INLINE iter #-}
iter :: (ReaderM m, Elem_kv key value) => RH m key value -> (a -> key -> value -> m a) -> a -> m a
iter (RH _ capacity _ _ hV eV) f a0 = go (Pos 0) a0
  where
    go !pos !a | unPos pos >= capacity = return a
               | otherwise = do
      h <- readHash hV pos
      case () of
        _ | h == Hash 0     -> go (Pos $ unPos pos + 1) a
          | isRemovedHash h -> go (Pos $ unPos pos + 1) a
          | otherwise -> do
              e <- readElem eV pos
              --(Elem _ k v) <- readElem eV pos
              let (_, k, v) = gt e
              !a' <- f a k v
              go (Pos $ unPos pos + 1) a'

toList :: (ReaderM m, Elem_kv key value) => RH m key value -> m [(key, value)]
toList rh = iter rh (\lst k v -> return ((k,v):lst)) []

remove :: (WriterM m, H.Hashable key, Eq key, Elem_kv key value)
       => RH m key value -> key -> m Bool
remove rh@(RH _ _ _ elemCount hV eV) key = do
  mpos <- lookupIndex rh key
  case mpos of
    Nothing -> return False
    Just pos -> do
      writeHash hV pos (mkRemovedHash (mkHash key))
      writeElem eV pos (error "removed element")
      modifyRef elemCount (\x -> x - (1::Int))
      return True

probeDistance :: RH s key value -> Hash -> Pos -> Int
probeDistance (RH mask capacity _ _ _ _) hash slot_index =
  (unPos slot_index + capacity - unPos (desiredPos mask hash)) .&. (unMask mask)

lookupIndex :: (ReaderM m, H.Hashable key, Eq key, Elem_kv key value)
            => RH m key value -> key -> m (Maybe Pos)
lookupIndex rh@(RH mask _ _ _ hV eV) key = go (desiredPos mask h0) 0
  where
    h0 = mkHash key
    go !pos !dist = do
      h <- readHash hV pos
      case () of
        _ | h == Hash 0                   -> return Nothing
          | dist > probeDistance rh h pos -> return Nothing
          | isRemovedHash h               -> go (incPos mask pos) (dist+1)
          | otherwise -> do
              e <- readElem eV pos
              let (_, key', _) = gt e
              --(Elem _ key' _) <- readElem eV pos
              if h == h0 && key == key'
                then return (Just pos)
                else go (incPos mask pos) (dist + 1)

lookup :: (ReaderM m, H.Hashable key, Eq key, Elem_kv key value)
       => RH m key value -> key -> m (Maybe value)
lookup rh key = do
  mpos <- lookupIndex rh key
  case mpos of
    Nothing -> return Nothing
    Just pos -> do
      e <-  readElem (_elemVector rh) pos
      let (_,_,value) = gt e
      -- (Elem _ _ value) <- readElem (_elemVector rh) pos
      return $! Just value

member :: (ReaderM m, H.Hashable key, Eq key, Elem_kv key value)
       => RH m key value -> key -> m Bool
member rh key = do
  mpos <- lookupIndex rh key
  return $! maybe False (const True) mpos

size :: (ReaderM m) => RH m key value -> m Int
size rh = readRef (_elemCount rh)

averageProbeCount :: (ReaderM m) => RH m key value -> m Double
averageProbeCount rh = go (Pos 0) 0
  where
    go !pos !acc
      | unPos pos >= _capacity rh = do
          (cnt :: Int) <- readRef (_elemCount rh)
          return (acc / (fromIntegral cnt + 1))
      | otherwise = do
          h <- readHash (_hashVector rh) pos
          let pd | h == Hash 0 = 0
                 | isRemovedHash h = 0
                 | otherwise = fromIntegral $ probeDistance rh h pos
          go (Pos $ unPos pos + 1) (acc + pd)

load :: (ReaderM m) => RH m key value -> m Double
load rh = do
  (cnt :: Int) <- readRef (_elemCount rh)
  return (fromIntegral cnt / fromIntegral (_capacity rh))

readHash :: (ReaderM m) => UnboxedArray m Int -> Pos -> m Hash
readHash hV pos = do
  h <- readUnboxedArrayIndex hV (unPos pos)
  return $! Hash h

readElem :: (ReaderM m) => BoxedArray m (ELEM_kv key value) -> Pos -> m (ELEM_kv key value)
readElem eV pos = readBoxedArrayIndex eV (unPos pos)

writeHash :: (WriterM m) => UnboxedArray m Int -> Pos -> Hash -> m ()
writeHash v pos hash = writeUnboxedArrayIndex v (unPos pos) (unHash hash)

writeElem :: (WriterM m) => BoxedArray m (ELEM_kv key value) -> Pos -> ELEM_kv key value -> m ()
writeElem v pos e = writeBoxedArrayIndex v (unPos pos) e

desiredPos :: Mask -> Hash -> Pos
desiredPos (Mask mask) (Hash hash) = Pos (mask .&. hash)

incPos :: Mask -> Pos -> Pos
incPos (Mask mask) (Pos pos) = Pos $ (pos + 1) .&. mask

isRemovedHash :: Hash -> Bool
isRemovedHash (Hash h) = h .&. minBound /= 0

mkRemovedHash :: Hash -> Hash
mkRemovedHash (Hash h) = Hash $ h .|. minBound

mkHash :: (H.Hashable k) => k -> Hash
mkHash k =
  let !h = H.hash k .&. maxBound
      !e | h == 0 = 1
         | otherwise = 0
      !h' = h .|. e
  in Hash h'