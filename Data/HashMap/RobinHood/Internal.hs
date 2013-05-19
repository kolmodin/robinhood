{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HashMap.RobinHood.Internal
  ( RH(..)
  , new
  , newWithCapacity
  , remove
  , insert
  , Data.HashMap.RobinHood.Internal.lookup
  , member
  , lookupIndex
  -- , toList
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
                         , _elemVector      :: !(BoxedArray m (Elem key value))
                         }

data Elem key value = Elem {-# UNPACK #-} !Hash !key !value

newtype Mask = Mask { unMask :: Int } deriving (Eq, Show)
newtype Hash = Hash { unHash :: Int } deriving (Eq, Show)
newtype Pos = Pos { unPos :: Int } deriving (Eq, Show)

lOAD_FACTOR_PERCENT :: Int
lOAD_FACTOR_PERCENT = 90

new :: (WriterM m) => m (RH m key value)
new = alloc 16

newWithCapacity :: (WriterM m) => Int -> m (RH m key value)
newWithCapacity cap0 = alloc cap
  where
    cap = head [ n | n <- iterate (*2) 16
                   , n * lOAD_FACTOR_PERCENT > cap0 * 100 ]

alloc :: (WriterM m) => Int -> m (RH m key value)
alloc !capacity = do
  h <- newUnboxedArray capacity 0
  e <- newBoxedArray capacity
  count <- newRef 0
  let mask = Mask (capacity - 1)
      resizeThreshold = capacity * lOAD_FACTOR_PERCENT `div` 100
  return (RH mask capacity resizeThreshold count h e)

grow :: (WriterM m, H.Hashable key)
     => (RH m key value) -> m (RH m key value)
grow rh0 = do
  rhNew <- alloc (_capacity rh0 * 2)
  iter rh0 (\k v -> insertHelper rhNew k v)
  return rhNew

{-# INLINE insert #-}
insert :: (WriterM m, H.Hashable key) => RH m key value -> key -> value -> m (Maybe (RH m key value))
insert rh@(RH _ _ resizeThreshold elemCount _ _) key value = do
  cnt <- readRef elemCount
  if (cnt + 1 >= resizeThreshold)
    then do
      rh' <- grow rh
      insertHelper rh' key value
      return $! (Just rh')
    else do
      insertHelper rh key value
      return $! Nothing

{-# INLINE insertHelper #-}
insertHelper :: (WriterM m, H.Hashable key) => RH m key value -> key -> value -> m ()
insertHelper rh@(RH mask _ _ elemCount hV eV) key0 value0 = do
  go hash0 (Elem hash0 key0 value0) pos0 0
  modifyRef elemCount (+1)
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
                     e'@(Elem h' _ _) <- readElem eV pos
                     put pos hash e
                     go h' e' (incPos mask pos) (existing_elem_probe_dist + 1)
          | otherwise -> go hash e (incPos mask pos) (dist + 1)
    put !pos !hash !e = do
      writeHash hV pos hash
      writeElem eV pos e
      return ()

{-# INLINE iter #-}
iter :: (ReaderM m) => RH m key value -> (key -> value -> m ()) -> m ()
iter (RH _ capacity _ _ hV eV) f = go (Pos 0)
  where
    go !pos | unPos pos >= capacity = return ()
            | otherwise = do
      h <- readHash hV pos
      case () of
        _ | h == Hash 0     -> go (Pos $ unPos pos + 1)
          | isRemovedHash h -> go (Pos $ unPos pos + 1)
          | otherwise -> do
              (Elem _ k v) <- readElem eV pos
              f k v
              go (Pos $ unPos pos + 1)

-- toList :: (ReaderM m) => RH m key value -> m [(key, value)]
-- toList rh = iter rh (\lst k v -> return ((k,v):lst)) []

remove :: (WriterM m, H.Hashable key, Eq key)
       => RH m key value -> key -> m Bool
remove rh@(RH _ _ _ elemCount hV eV) key = do
  mpos <- lookupIndex rh key
  case mpos of
    Nothing -> return False
    Just pos -> do
      writeHash hV pos (mkRemovedHash (mkHash key))
      writeElem eV pos (error "removed element")
      modifyRef elemCount (\x -> x - 1)
      return True

probeDistance :: RH s key value -> Hash -> Pos -> Int
probeDistance (RH mask capacity _ _ _ _) hash slot_index =
  (unPos slot_index + capacity - unPos (desiredPos mask hash)) .&. (unMask mask)

lookupIndex :: (ReaderM m, H.Hashable key, Eq key)
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
              (Elem _ key' _) <- readElem eV pos
              if h == h0 && key == key'
                then return (Just pos)
                else go (incPos mask pos) (dist + 1)

lookup :: (ReaderM m, H.Hashable key, Eq key)
       => RH m key value -> key -> m (Maybe value)
lookup rh key = do
  mpos <- lookupIndex rh key
  case mpos of
    Nothing -> return Nothing
    Just pos -> do
      (Elem _ _ value) <- readElem (_elemVector rh) pos
      return $! Just value

member :: (ReaderM m, H.Hashable key, Eq key)
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
          cnt <- readRef (_elemCount rh)
          return (acc / (fromIntegral cnt + 1))
      | otherwise = do
          h <- readHash (_hashVector rh) pos
          let pd | h == Hash 0 = 0
                 | isRemovedHash h = 0
                 | otherwise = fromIntegral $ probeDistance rh h pos
          go (Pos $ unPos pos + 1) (acc + pd)

load :: (ReaderM m) => RH m key value -> m Double
load rh = do
  cnt <- readRef (_elemCount rh)
  return (fromIntegral cnt / fromIntegral (_capacity rh))

readHash :: (ReaderM m) => UnboxedArray m Int -> Pos -> m Hash
readHash hV pos = do
  h <- readUnboxedArrayIndex hV (unPos pos)
  return $! Hash h

readElem :: (ReaderM m) => BoxedArray m (Elem key value) -> Pos -> m (Elem key value)
readElem eV pos = readBoxedArrayIndex eV (unPos pos)

writeHash :: (WriterM m) => UnboxedArray m Int -> Pos -> Hash -> m ()
writeHash v pos hash = writeUnboxedArrayIndex v (unPos pos) (unHash hash)

writeElem :: (WriterM m) => BoxedArray m (Elem key value) -> Pos -> Elem key value -> m ()
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