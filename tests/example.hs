{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Monad               (forM_)
import           GHC.Stats
import           System.Mem

import qualified Data.HashMap.Strict         as HM

import qualified Data.HashMap.RobinHood.Base as RH

main :: IO ()
main = do
  rh <- do
    rh <- RH.new
    let loop2 !n !rh | n > 100000 = return rh
                     | otherwise = do
                          !rh' <- RH.insert rh n ()
                          loop2 (n+1) rh'
    loop2 (0::Int) rh

  forM_ [0..100000] $ \n -> do
    e <- RH.member rh n
    if not e
      then print n
      else return ()

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

  RH.size rh >>= print
  RH.averageProbeCount rh >>= print
  RH.load rh >>= print
  RH.lookup rh 1984 >>= print
