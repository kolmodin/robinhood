{-# LANGUAGE Rank2Types #-}

module Data.HashMap.RobinHood.ST where

import           Control.Monad.Identity
import           Control.Monad.ST            (ST, runST)

import qualified Data.Vector.Unboxed as UMV
import qualified Data.Vector as BMV

import qualified Data.Hashable as H

import qualified Data.HashMap.RobinHood.Base as Base
import Data.HashMap.RobinHood.Base (RH(..))
import Data.HashMap.RobinHood.Ref

type PureRH key value = RH Identity key value

makeRobinHoodST :: (forall s. ST s (Base.RH (ST s) key value)) -> PureRH key value
makeRobinHoodST m = runST $ do
  rh <- m
  elemCount <- readRef (_elemCount rh)
  let elemCountRef = runIdentity $ newRef elemCount
  hV <- UMV.unsafeFreeze (_hashVector rh)
  eV <- BMV.unsafeFreeze (_elemVector rh)

  return $! (RH { _mask = _mask rh
                , _capacity = _capacity rh
                , _resizeThreshold = _resizeThreshold rh
                , _elemCount =  elemCountRef
                , _hashVector = hV
                , _elemVector = eV
                })

makeRobinHoodFromList :: (H.Hashable key, Base.Elem_kv key value)
                      => [(key,value)] -> PureRH key value
makeRobinHoodFromList lst = makeRobinHoodST $ do
    rh0 <- Base.new
    foldM (\rh' (k,v) -> Base.insert rh' k v) rh0 lst

lookup :: (Base.Elem_kv key value, H.Hashable key, Eq key)
       => PureRH key value -> key -> Maybe value
lookup rh k = runIdentity $ Base.lookup rh k