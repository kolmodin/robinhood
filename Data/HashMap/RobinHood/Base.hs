
module Data.HashMap.RobinHood.Base
  ( Data.HashMap.RobinHood.Base.RH(..)
  , new
  , newWithCapacity
  , insert
  , Data.HashMap.RobinHood.Base.lookup
  , member
  , size
  , load
  , averageProbeCount
  ) where

import qualified Data.Hashable as H

import qualified Data.HashMap.RobinHood.Internal as Internal
import           Data.HashMap.RobinHood.Monad
import           Data.HashMap.RobinHood.Ref

data RH m key value = RH { unRH :: Ref m (Internal.RH m key value) }

new :: (WriterM m) => m (RH m key value)
new = do
  rh <- Internal.new
  rhRef <- newRef rh
  return $! RH rhRef

newWithCapacity :: (WriterM m) => Int -> m (RH m key value)
newWithCapacity cap = do
  rh <- Internal.newWithCapacity cap
  rhRef <- newRef rh
  return $! RH rhRef

insert :: (WriterM m, H.Hashable key) => RH m key value -> key -> value -> m ()
insert (RH ref) key value = do
   rh <- readRef ref
   rhM <- Internal.insert rh key value
   case rhM of
     Just rh' -> writeRef ref rh'
     Nothing -> return ()

lookup :: (ReaderM m, H.Hashable key, Eq key)
       => RH m key value -> key -> m (Maybe value)
lookup (RH ref) key = do
  rh <- readRef ref
  Internal.lookup rh key

member :: (ReaderM m, H.Hashable key, Eq key)
       => RH m key value -> key -> m Bool
member (RH ref) key = do
  rh <- readRef ref
  Internal.member rh key

size :: (ReaderM m) => RH m key value -> m Int
size (RH ref) = Internal.size =<< readRef ref

load :: (ReaderM m) => RH m key value -> m Double
load (RH ref) = Internal.load =<< readRef ref

averageProbeCount :: (ReaderM m) => RH m key value -> m Double
averageProbeCount (RH ref) = Internal.averageProbeCount =<< readRef ref