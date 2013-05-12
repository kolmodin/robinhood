{-# LANGUAGE TypeFamilies #-}

module Data.HashMap.RobinHood.Ref
  ( ReadRef(..)
  , WriteRef(..)
  ) where

import           Control.Monad.Identity
import           Control.Monad.ST        (ST)
import           Data.IORef              (IORef, modifyIORef', newIORef,
                                          readIORef, writeIORef)
import           Data.STRef              (STRef, modifySTRef', newSTRef,
                                          readSTRef, writeSTRef)

import           Control.Monad.Primitive (PrimMonad)

newtype Id a = Id { unId :: a } deriving (Eq, Show)

class (Monad m) => ReadRef m where
  type Ref m :: * -> *
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a

class (ReadRef m, PrimMonad m) => WriteRef m where
  writeRef :: Ref m a -> a -> m ()
  modifyRef :: Ref m a -> (a -> a) -> m ()

instance ReadRef Identity where
	type Ref Identity = Id
	newRef = return . Id
	readRef = return . unId

instance ReadRef IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef

instance WriteRef IO where
  writeRef = writeIORef
  modifyRef = modifyIORef'

instance ReadRef (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef

instance WriteRef (ST s) where
  writeRef = writeSTRef
  modifyRef = modifySTRef'
