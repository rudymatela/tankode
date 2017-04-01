-- |
-- Module      : Tankode.Basic0
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- A basic interface for implementing Tankodes in Haskell.
--
-- It provides everything from the "Tankode.Raw" interface and
-- keeps track of the 'gunHeading' and 'radarHeading'.
--
-- For the future:
-- implement the Basic1 (... BasicN) interfaces that:
--
-- * keep track of the expected speed of the Tankode 
--   if the next speed sent from the server is different
--   it means that we hit an obstacle.
--
-- * keep track of the current gun power.
module Tankode.Basic0
  ( Id (..), Colour, IncDec (..)
  , Input (..), Output (..), output
  , Tankode, TankodeIO, run, runIO
  , module Tankode.Constants
  )
where

import Data.Ratio
import Data.Maybe
import Control.Monad
import System.IO
import qualified Tankode.Raw1 as Raw
import Tankode.Raw (Id(..), Colour, IncDec(..))
import Tankode.Constants

type Tankode s   = Input s ->     Output s
type TankodeIO s = Input s -> IO (Output s)

data Input s = Input
  { life   :: Rational
  , speed  :: Rational
  , enemy  :: Maybe Rational
  , wall   :: Maybe Rational
  , gunHeading   :: Rational
  , radarHeading :: Rational
  , istate :: s
  }
  deriving Show

data Output s = Output
  { accel  :: IncDec
  , body   :: IncDec
  , gun    :: Rational
  , radar  :: Rational
  , shoot  :: Rational
  , ostate :: s
  }
  deriving Show

output :: Output s
output = Output
  { accel  = Nul
  , body   = Nul
  , gun    = 0
  , radar  = 0
  , shoot  = 0
  , ostate = error "output: undefined state"
  }

fromRawInput :: Raw.Input (Rational,Rational,s) -> Input s
fromRawInput rawInput = Input
  { life  = Raw.life  rawInput
  , speed = Raw.speed rawInput
  , enemy = Raw.enemy rawInput
  , wall  = Raw.wall  rawInput
  , gunHeading   = g
  , radarHeading = r
  , istate       = s
  }
  where
  (g,r,s) = Raw.istate rawInput

toRawOutput :: Rational -> Rational -> Output s -> Raw.Output (Rational,Rational,s)
toRawOutput g r output = Raw.Output
  { Raw.accel  = accel output
  , Raw.body   = body  output
  , Raw.gun    = gun   output
  , Raw.radar  = radar output
  , Raw.shoot  = shoot output
  , Raw.ostate = (g + gun output,r + radar output,ostate output)
  }

toRaw :: Tankode s -> Raw.Tankode (Rational,Rational,s)
toRaw tk rin = toRawOutput g r out
  where
  out = tk $ fromRawInput rin
  (g,r,_) = Raw.istate rin

toRawIO :: TankodeIO s -> Raw.TankodeIO (Rational,Rational,s)
toRawIO tk rin = do
  let (g,r,_) = Raw.istate rin
  out <- tk $ fromRawInput rin
  return $ toRawOutput g r out

run :: Id -> Tankode s -> s -> IO ()
run id tk s0 = Raw.run id (toRaw tk) (0,0,s0)

runIO :: Id -> TankodeIO s -> s -> IO ()
runIO id tk s0 = Raw.runIO id (toRawIO tk) (0,0,s0)
