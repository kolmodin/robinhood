{-# LANGUAGE TypeFamilies         #-}

module Data.HashMap.RobinHood.Monad where

import           Control.Monad.Identity
import           Control.Monad.ST            (ST)

import           Control.Monad.Primitive     (PrimMonad (..), RealWorld)
import qualified Data.Vector                 as BV
import qualified Data.Vector.Mutable         as BMV
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

import           Data.HashMap.RobinHood.Ref

class (Monad m, ReadRef m) => ReaderM m where
  type UnboxedArray m :: * -> *
  type BoxedArray m :: * -> *
  readUnboxedArrayIndex :: (UV.Unbox a) => (UnboxedArray m a) -> Int -> m a
  readBoxedArrayIndex :: (BoxedArray m a) -> Int -> m a

instance ReaderM Identity where
  type UnboxedArray Identity = UV.Vector
  type BoxedArray Identity = BV.Vector
  readUnboxedArrayIndex v ix = Identity (UV.unsafeIndex v ix)
  readBoxedArrayIndex v ix = Identity (BV.unsafeIndex v ix)

instance ReaderM IO where
  type UnboxedArray IO = UMV.MVector RealWorld
  type BoxedArray IO = BMV.MVector RealWorld
  readUnboxedArrayIndex v ix = UMV.unsafeRead v ix
  readBoxedArrayIndex v ix = BMV.unsafeRead v ix

instance ReaderM (ST s) where
  type UnboxedArray (ST s) = UMV.MVector s
  type BoxedArray (ST s) = BMV.MVector s
  readUnboxedArrayIndex v ix = UMV.unsafeRead v ix
  readBoxedArrayIndex v ix = BMV.unsafeRead v ix

class (ReaderM m, WriteRef m, PrimMonad m) => WriterM m where
  newUnboxedArray :: (UV.Unbox a) => Int -> a -> m (UnboxedArray m a)
  newBoxedArray :: Int -> m (BoxedArray m a)
  writeUnboxedArrayIndex :: (UV.Unbox a) => UnboxedArray m a -> Int -> a -> m ()
  writeBoxedArrayIndex :: BoxedArray m a -> Int -> a -> m ()

instance WriterM IO where
  newUnboxedArray cap v = UMV.replicate cap v
  newBoxedArray cap = BMV.new cap
  writeUnboxedArrayIndex v ix = UMV.unsafeWrite v ix
  writeBoxedArrayIndex v ix = BMV.unsafeWrite v ix

instance WriterM (ST s) where
  newUnboxedArray cap v = UMV.replicate cap v
  newBoxedArray cap = BMV.new cap
  writeUnboxedArrayIndex v ix = UMV.unsafeWrite v ix
  writeBoxedArrayIndex v ix = BMV.unsafeWrite v ix
