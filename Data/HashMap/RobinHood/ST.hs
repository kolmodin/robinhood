{-# LANGUAGE Rank2Types #-}

module Data.HashMap.RobinHood.ST where

import           Control.Monad.Identity
import           Control.Monad.ST            (ST, runST)

import qualified Data.Vector                 as BMV
import qualified Data.Vector.Unboxed         as UMV

import qualified Data.HashMap.RobinHood.Base as Base
import qualified Data.HashMap.RobinHood.Internal as Internal
import           Data.HashMap.RobinHood.Ref

type PureRH key value = Base.RH Identity key value

makeRobinHoodST :: (forall s. ST s (Base.RH (ST s) key value)) -> PureRH key value
makeRobinHoodST m = runST $ do
  rhRef <- m
  rh <- readRef (Base.unRH rhRef)
  elemCount <- readRef (Internal._elemCount rh)
  let elemCountRef = runIdentity $ newRef elemCount
  hV <- UMV.unsafeFreeze (Internal._hashVector rh)
  eV <- BMV.unsafeFreeze (Internal._elemVector rh)

  return $! Base.RH . runIdentity . newRef $!
      Internal.RH { Internal._mask = Internal._mask rh
                  , Internal._capacity = Internal._capacity rh
                  , Internal._resizeThreshold = Internal._resizeThreshold rh
                  , Internal._elemCount =  elemCountRef
                  , Internal._hashVector = hV
                  , Internal._elemVector = eV
                  }
