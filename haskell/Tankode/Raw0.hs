-- |
-- Module      : Tankode.Raw0
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- A raw interface for implementing tankodes.
--
-- 'Input' is just what the server sent,
-- 'Output' is just what will be sent to the server.
--
-- The "Tankode.Basic" interface is recommended for beginners.
module Tankode.Raw0
  ( Id (..), Colour, IncDec (..)
  , Input (..), Output (..), output
  , Tankode, TankodeIO, run, runIO
  , module Tankode.Constants
  )
where

import Tankode.Common
import Data.Ratio
import Data.Maybe
import Control.Monad
import System.IO
import Tankode.Constants

data Input  = Input
  { life  :: Rational
  , speed :: Rational
  , enemy :: Maybe Rational
  , wall  :: Maybe Rational
  }
  deriving Show

data Output = Output
  { accel :: IncDec
  , body  :: IncDec
  , gun   :: Rational
  , radar :: Rational
  , shoot :: Rational
  }
  deriving Show

output :: Output
output = Output
  { accel = Nul
  , body  = Nul
  , gun   = 0
  , radar = 0
  , shoot = 0
  }

type Tankode s   = Input -> s ->    (s,Output)
type TankodeIO s = Input -> s -> IO (s,Output)
type TankodeList = [Input] -> [Output]

run :: Id -> Tankode s -> s -> IO ()
run ident tk = runIO ident tkIO
  where
  tkIO i s = return $ tk i s

runIO :: Id -> TankodeIO s -> s -> IO ()
runIO ident tk s0 = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  putStrLn (showId ident)
  r s0
  where
  r s = unlessEOF $ do
    input <- get
    when (life input > 0) $ do
      (s',output) <- tk input s
      put output
      r s'

runList :: TankodeList -> IO ()
runList = undefined -- TODO: implement me

get :: IO Input
get = readInput <$> getLine

put :: Output -> IO ()
put = putStrLn . showOutput

showId :: Id -> String
showId i = unwords
  [ name         i
  , trackColour  i
  , bodyColour   i
  , gunColour    i
  , radarColour  i
  , bulletColour i
  , scanColour   i
  ]

readInput :: String -> Input
readInput str = Input
  { life  = readR l
  , speed = readR s
  , enemy = readMR e
  , wall  = readMR  w
  }
  where
  [l,s,e,w] = words str

showOutput :: Output -> String
showOutput o = unwords
  [ showIncDec $ accel o
  , showIncDec $ body  o
  , showR $ gun   o `min` maxGunTurn   `max` (-maxGunTurn)
  , showR $ radar o `min` maxRadarTurn `max` (-maxRadarTurn)
  , showR $ shoot o `min` 1 `max` 0
  ]

showR :: Rational -> String
showR r = show (numerator r) ++ "/" ++ show (denominator r)

showIncDec :: IncDec -> String
showIncDec Inc = "+"
showIncDec Dec = "-"
showIncDec Nul = "="

readR :: String -> Rational
readR = fromJust . readMR

readMR :: String -> Maybe Rational
readMR s =
  case span (/= '/') s of
    (n,'/':d) -> Just $ read n % read d
    _         -> Nothing

unlessEOF :: IO () -> IO ()
unlessEOF a = do
  end <- isEOF
  unless end a
