module Tankode.Raw
  ( Id (..)
  , Input (..), Output (..), output
  , Tankode, TankodeIO
  , run, runIO
  , module Tankode.Constants
  )
where

import Data.Ratio
import Data.Maybe
import Control.Monad
import System.IO
import Tankode.Constants

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

-- TODO: implement me
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
  , showR $ gun   o
  , showR $ radar o
  , showR $ shoot o
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
