{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main, new, remove, insert, Main.lookup, lookupIndex, toList, desiredPos, mkHash) where

import qualified Data.Hashable               as H
import qualified Data.Vector.Mutable         as V
import qualified Data.Vector.Unboxed.Mutable as U

import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)

import           Control.Monad.Primitive     (PrimMonad (..))
import           Data.STRef                  (STRef, newSTRef, readSTRef,
                                              writeSTRef)
import Control.Monad.ST (ST)

import           Data.Bits                   ((.&.), (.|.))

import GHC.Stats
import System.Mem

import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HM

data RH m key value = RH { _mask            :: {-# UNPACK #-} !Mask
                         , _capacity        :: {-# UNPACK #-} !Int
                         , _resizeThreshold :: {-# UNPACK #-} !Int
                         , _elemCount       :: RefType m Int
                         , _hashVector      :: U.MVector (PrimState m) Int
                         , _elemVector      :: V.MVector (PrimState m) (ELEM_kv key value)
                         }


data Elem key value = Elem {-# UNPACK #-} !Hash !key !value

class Elem_kv k v where
  data ELEM_kv k v
  mk :: Hash -> k -> v -> ELEM_kv k v
  gt :: ELEM_kv k v -> (Hash, k, v)

instance Elem_kv Int () where
  data ELEM_kv Int () = ELEM_Int {-# UNPACK #-} !Int
  mk h k v = ELEM_Int k
  gt (ELEM_Int k) = (mkHash k, k, ())

newtype Mask = Mask { unMask :: Int } deriving (Eq, Show)
newtype Hash = Hash { unHash :: Int } deriving (Eq, Show)
newtype Pos = Pos { unPos :: Int } deriving (Eq, Show)

lOAD_FACTOR_PERCENT :: Int
lOAD_FACTOR_PERCENT = 90

new :: (PrimMonad m, Ref m) => m (RH m key value)
new = alloc 16

alloc :: (PrimMonad m, Ref m) => Int -> m (RH m key value)
alloc !capacity = do
  h <- U.replicate capacity 0
  e <- V.new capacity
  count <- newRef 0
  let mask = Mask (capacity - 1)
      resizeThreshold = capacity * lOAD_FACTOR_PERCENT `div` 100
  return (RH mask capacity resizeThreshold count h e)

grow :: (PrimMonad m, Ref m, H.Hashable key, Elem_kv key value)
     => (RH m key value) -> m (RH m key value)
grow rh0 = do
  rhNew <- alloc (_capacity rh0 * 2)
  iter rh0 (\rh k v -> insert rh k v) rhNew

{-# INLINE insert #-}
insert :: (PrimMonad m, Ref m, H.Hashable key, Elem_kv key value) => RH m key value -> key -> value -> m (RH m key value)
insert rh@(RH _ _ resizeThreshold elemCount _ _) key value = do
  cnt <- readRef elemCount
  if (cnt + 1 >= resizeThreshold)
    then do
      rh' <- grow rh
      insertHelper rh' key value
      return rh'
    else insertHelper rh key value >> return rh

{-# INLINE insertHelper #-}
insertHelper :: (PrimMonad m, Ref m, H.Hashable key, Elem_kv key value) => RH m key value -> key -> value -> m ()
insertHelper rh@(RH mask _ _ elemCount hV eV) key0 value0 = do
  let !e0 = mk hash0 key0 value0
  go hash0 e0 pos0 0
  cnt <- readRef elemCount
  writeRef elemCount $! cnt+1
  where
    hash0 = mkHash key0
    pos0 = desiredPos mask hash0
    go !hash !e !pos !dist = do
      hash' <- readHash hV pos
      if hash' == Hash 0
        then put pos hash e
        else do
      let existing_elem_probe_dist = probeDistance rh hash' pos
      if existing_elem_probe_dist < dist
        then if isRemovedHash hash'
               then put pos hash e
               else do
                 eA <- readElem eV pos
                 let (h', _, _) = gt eA
                 -- e'@(Elem h' _ _) <- readElem eV pos
                 put pos hash e
                 go h' eA (incPos mask pos) (existing_elem_probe_dist + 1)
        else go hash e (incPos mask pos) (dist + 1)
    put !pos !hash !e = do
      writeHash hV pos hash
      writeElem eV pos e
      return ()

{-# INLINE iter #-}
iter :: (PrimMonad m, Elem_kv key value) => RH m key value -> (a -> key -> value -> m a) -> a -> m a
iter (RH _ capacity _ _ hV eV) f a0 = go (Pos 0) a0
  where
    go !pos !a | unPos pos >= capacity = return a
               | otherwise = do
      h <- readHash hV pos
      if h == Hash 0
        then go (Pos $ unPos pos + 1) a
        else do
      if isRemovedHash h
        then go (Pos $ unPos pos + 1) a
        else do
      e <- readElem eV pos
      --(Elem _ k v) <- readElem eV pos
      let (_, k, v) = gt e
      !a' <- f a k v
      go (Pos $ unPos pos + 1) a'

toList :: (PrimMonad m, Elem_kv key value) => RH m key value -> m [(key, value)]
toList rh = iter rh (\lst k v -> return ((k,v):lst)) []

remove :: (PrimMonad m, Ref m, H.Hashable key, Eq key, Elem_kv key value)
       => RH m key value -> key -> m Bool
remove rh@(RH _ _ _ elemCount hV eV) key = do
  mpos <- lookupIndex rh key
  case mpos of
    Nothing -> return False
    Just pos -> do
      writeHash hV pos (mkRemovedHash (mkHash key))
      writeElem eV pos (error "removed element")
      cnt <- readRef elemCount
      writeRef elemCount (cnt-1)
      return True

probeDistance :: RH s key value -> Hash -> Pos -> Int
probeDistance (RH mask capacity _ _ _ _) hash slot_index =
  (unPos slot_index + capacity - unPos (desiredPos mask hash)) .&. (unMask mask)

lookupIndex :: (H.Hashable key, Eq key, PrimMonad m, Elem_kv key value)
            => RH m key value -> key -> m (Maybe Pos)
lookupIndex rh@(RH mask _ _ _ hV eV) key = go (desiredPos mask h0) 0
  where
    h0 = mkHash key
    go !pos !dist = do
      h <- readHash hV pos
      if h == Hash 0
        then return Nothing
        else do
      if dist > probeDistance rh h pos
        then return Nothing
        else do
      if isRemovedHash h
        then go (incPos mask pos) (dist+1)
        else do
      e <- readElem eV pos
      let (_, key', _) = gt e
      --(Elem _ key' _) <- readElem eV pos
      if h == h0 && key == key'
        then return (Just pos)
        else go (incPos mask pos) (dist + 1)

lookup :: (PrimMonad m, H.Hashable key, Eq key, Elem_kv key value)
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

member :: (PrimMonad m, H.Hashable key, Eq key, Elem_kv key value)
       => RH m key value -> key -> m Bool
member rh key = do
  mpos <- lookupIndex rh key
  return $! maybe False (const True) mpos

size :: (Ref m) => RH m key value -> m Int
size rh = readRef (_elemCount rh)

averageProbeCount :: (PrimMonad m, Ref m) => RH m key value -> m Double
averageProbeCount rh = go (Pos 0) 0
  where
    go !pos !s | unPos pos >= _capacity rh = do
                   cnt <- readRef (_elemCount rh)
                   return (s / (fromIntegral cnt + 1))
               | otherwise = do
      h <- readHash (_hashVector rh) pos
      let d | h == Hash 0 = 0
            | isRemovedHash h = 0
            | otherwise = fromIntegral $ probeDistance rh h pos
      go (Pos $ unPos pos + 1) (s + d)

load :: (Monad m, Ref m) => RH m key value -> m Double
load rh = do
  cnt <- readRef (_elemCount rh)
  return (fromIntegral cnt / fromIntegral (_capacity rh))

readHash :: (PrimMonad m) => U.MVector (PrimState m) Int -> Pos -> m Hash
readHash hV pos = do
  h <- U.unsafeRead hV (unPos pos)
  return $! Hash h

readElem :: (PrimMonad m) => V.MVector (PrimState m) (ELEM_kv key value) -> Pos -> m (ELEM_kv key value)
readElem eV pos = V.unsafeRead eV (unPos pos)

writeHash :: (PrimMonad m) => U.MVector (PrimState m) Int -> Pos -> Hash -> m ()
writeHash v pos hash = U.unsafeWrite v (unPos pos) (unHash hash)

writeElem :: (PrimMonad m) => V.MVector (PrimState m) (ELEM_kv key value) -> Pos -> ELEM_kv key value -> m ()
writeElem v pos e = V.unsafeWrite v (unPos pos) e

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

class Ref m where
  type RefType m :: * -> *
  newRef :: a -> m (RefType m a)
  readRef :: RefType m a -> m a
  writeRef :: RefType m a -> a -> m ()

instance Ref IO where
  type RefType IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance Ref (ST s) where
  type RefType (ST s)= STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

main :: IO ()
main = do
  rh0 <- new
  let loop2 !n !rh | n > 10000000 = return rh
                   | otherwise = do
                      rh' <- insert rh n ()
                      loop2 (n+1) rh'
  rh' <- loop2 (0::Int) rh0
  
  forM_ [0..10000000] $ \n -> do
    e <- member rh' n
    if not e
      then print n
      else return ()
      
  {-
  let loop !th !n | n > 10000000 = th
                  | otherwise = loop (HM.insert n () th) (n+1)

      !th = loop HM.empty (0::Int)
      !n = [ n | n <- [0..10000000], not $ HM.member n th]

  print (take 10 n)
  print (HM.size th)
  -}
  statsOn <- getGCStatsEnabled
  if statsOn
    then do
      performGC
      stats <- getGCStats
      putStrLn "stats"
      print (currentBytesUsed stats)
    else putStrLn "stats not enabled"
  
  -- print (HM.size th)

  size rh' >>= print
  averageProbeCount rh' >>= print
  load rh' >>= print
  
  return ()
