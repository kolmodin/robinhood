{-# LANGUAGE TypeFamilies #-}

module Data.HashMap.RobinHood.Ref where


import           Control.Monad.Identity
import           Control.Monad.ST       (ST)
import           Data.IORef             (IORef, modifyIORef', newIORef,
                                         readIORef, writeIORef)
import           Data.STRef             (STRef, modifySTRef', newSTRef,
                                         readSTRef, writeSTRef)

import           Control.Monad.Primitive     (PrimMonad (..))


class (Monad m) => ReadRef m where
  type Ref m a
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a

class (ReadRef m, PrimMonad m) => WriteRef m where
  writeRef :: Ref m a -> a -> m ()
  modifyRef :: Ref m a -> (a -> a) -> m ()

instance ReadRef Identity where
	type Ref Identity a = a
	newRef = return
	readRef = return

instance ReadRef IO where
  type Ref IO a = IORef a
  newRef = newIORef
  readRef = readIORef

instance WriteRef IO where
  writeRef = writeIORef
  modifyRef = modifyIORef'

instance ReadRef (ST s) where
  type Ref (ST s) a = STRef s a
  newRef = newSTRef
  readRef = readSTRef

instance WriteRef (ST s) where
  writeRef = writeSTRef
  modifyRef = modifySTRef'
