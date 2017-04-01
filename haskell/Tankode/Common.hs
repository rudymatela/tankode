-- |
-- Module      : Tankode.Common
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Common functions between different interfaces.
module Tankode.Common
  ( Id (..)
  , Colour
  , IncDec (..)
  )
where

type Colour = String

data IncDec = Dec | Nul | Inc
  deriving Show

instance Num IncDec where
  fromInteger x | x < 0     = Dec
                | x > 0     = Inc
                | otherwise = Nul
  Dec + Inc = Nul
  Dec + _   = Dec
  Nul + x   = x
  Inc + _   = Inc
  Nul * _   = Nul
  Inc * x   = x
  Dec * x   = negate x
  abs Dec = Inc
  abs x   = x
  signum = id
  negate Dec = Inc
  negate Nul = Nul
  negate Inc = Dec

data Id = Id
  { name         :: String
  , trackColour  :: Colour
  , bodyColour   :: Colour
  , gunColour    :: Colour
  , radarColour  :: Colour
  , bulletColour :: Colour
  , scanColour   :: Colour
  }
  deriving Show

