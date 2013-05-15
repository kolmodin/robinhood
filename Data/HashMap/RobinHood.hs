
module Data.HashMap.RobinHood where

-- import Control.Monad (foldM)
import Control.Monad.Identity

import qualified Data.Hashable as H

import Data.HashMap.RobinHood.ST
import qualified Data.HashMap.RobinHood.Base as Base

makeRobinHoodFromList :: (H.Hashable key)
                      => [(key,value)] -> PureRH key value
makeRobinHoodFromList lst = makeRobinHoodST $ do
    rh0 <- Base.newWithCapacity (length lst)
    foldM (\rh' (k,v) -> Base.insert rh' k v) rh0 lst

lookup :: ( H.Hashable key, Eq key)
       => PureRH key value -> key -> Maybe value
lookup rh k = runIdentity $ Base.lookup rh k

member :: (H.Hashable key, Eq key)
       => PureRH key value -> key -> Bool
member rh k = runIdentity $ Base.member rh k

size :: PureRH key value -> Int
size rh = runIdentity $ Base.size rh

load :: PureRH key value -> Double
load rh = runIdentity $ Base.load rh

averageProbeCount :: PureRH key value -> Double
averageProbeCount rh = runIdentity $ Base.averageProbeCount rh