-- |
-- Module      : Tankode.IPC
-- Copyright   : (c) 2016, 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Inter process communication with tankode AI processes.
module Tankode.IPC
  ( setupTankode
  , propagateSIGTERM
  , pipeTo
  )
where

import Tankode.Data
import Tankode.Show
import PipeRun
import System.IO
import Tankode.Constants
import Data.Ratio ((%))

-- runs a tankode and initializes necessary fields
setupTankode :: [String] -> IO (Maybe Tank)
setupTankode command = do
  (pid,i,o) <- pun command
  (n:t:b:g:r:u:s:_) <- words <$> hGetLine i
  return $ Just $ tank
    { name = n
    , trackColour   = readColour t
    , bodyColour    = readColour b
    , gunColour     = readColour g
    , radarColour   = readColour r
    , bulletColour  = readColour u
    , scanColour    = readColour s
    , tankode = tk i o
    , pid = pid
    }
  where
  tk i o (l,s,e,w) = do
    hPutStrLn o $ unwords [showR l, showR s, showMR e, showMR w]
    if l <= 0
      then return $ error "setupTankode: Tankode terminated"
      else do
        [a,b,g,r,s] <- words <$> hGetLine i
        return (readIncDec a, readIncDec b, readGunTurn g, readRadarTurn r, readShoot s)
-- TODO: detect errors (like file not found, could not execute)
-- to do that I'll need to modify `pun`

readIncDec :: String -> Rational
readIncDec "+" =  1
readIncDec "=" =  0
readIncDec "-" = -1

readGunTurn, readRadarTurn, readShoot :: String -> Rational
readGunTurn   s = readR s `max` (-maxGunTurn) `min` maxGunTurn
readRadarTurn s = readR s `max` (-maxRadarTurn) `min` maxRadarTurn
readShoot     s = readR s `max` 0 `min` maxShoot
