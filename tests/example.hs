{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad               (forM_)
import           GHC.Stats
import           System.Mem

import qualified Data.HashMap.Strict         as HM

import qualified Data.HashMap.RobinHood      as PureRH
import qualified Data.HashMap.RobinHood.Base as RH

main :: IO ()
main = do
  --rh <- do
  --  rh <- RH.new
  --  let loop2 !n !rh | n > 1000000 = return rh
  --                   | otherwise = do
  --                        !rh' <- RH.insert rh n ()
  --                        loop2 (n+1) rh'
  --  loop2 (0::Int) rh

  --forM_ [0..1000000] $ \n -> do
  --  e <- RH.member rh n
  --  if not e
  --    then print n
  --    else return ()

  let rh = PureRH.makeRobinHoodFromList [(n,()) | n <- [0..1000000::Int]]
  mapM_ print $ filter (not . PureRH.member rh) [0..1000000]

  statsOn <- getGCStatsEnabled
  if statsOn
    then do
      performGC
      stats <- getGCStats
      putStrLn "stats - current bytes used"
      print (currentBytesUsed stats)
      putStrLn "stats - bytes allocated"
      print (bytesAllocated stats)

    else putStrLn "stats not enabled"

  print (PureRH.size rh)
  print (PureRH.averageProbeCount rh)
  print (PureRH.load rh)
  print (PureRH.lookup rh 1984)
  --RH.size rh >>= print
  --RH.averageProbeCount rh >>= print
  --RH.load rh >>= print
  --RH.lookup rh 1984 >>= print
