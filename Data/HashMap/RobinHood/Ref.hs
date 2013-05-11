{-# LANGUAGE TypeFamilies #-}

module Data.HashMap.RobinHood.Ref where

import           Control.Monad.ST (ST)
import           Data.IORef       (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import           Data.STRef       (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef')

class Ref m where
  type RefType m :: * -> *
  newRef :: a -> m (RefType m a)
  readRef :: RefType m a -> m a
  writeRef :: RefType m a -> a -> m ()
  modifyRef :: RefType m a -> (a -> a) -> m ()

instance Ref IO where
  type RefType IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef'

instance Ref (ST s) where
  type RefType (ST s)= STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef'
