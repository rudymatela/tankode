module Tankode.Raw
  ( Id (..)
  , Input (..), Output (..), output
  , Tankode, TankodeIO
  , run, runIO
  )
where

import Data.Ratio
import Data.Maybe
import Control.Monad
import System.IO

type Colour = String

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
  { accel :: Rational
  , body  :: Rational
  , gun   :: Rational
  , radar :: Rational
  , shoot :: Rational
  }
  deriving Show

output :: Output
output = Output
  { accel = 0
  , body  = 0
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
showOutput o = unwords . map showR $
  [ accel o
  , body  o
  , gun   o
  , radar o
  , shoot o
  ]

showR :: Rational -> String
showR r = show (numerator r) ++ "/" ++ show (denominator r)

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
