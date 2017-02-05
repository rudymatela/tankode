module Tankode.Basic
  ( Id (..)
  , module Tankode.Constants
  )
where

import Data.Ratio
import Data.Maybe
import Control.Monad
import System.IO
import qualified Tankode.Raw as Raw
import Tankode.Raw (Id(..), Colour, IncDec(..))
import Tankode.Constants

type Tankode s   = Input -> s ->    (s,Output)
type TankodeIO s = Input -> s -> IO (s,Output)

data Input  = Input
  { life  :: Rational
  , speed :: Rational
  , enemy :: Maybe Rational
  , wall  :: Maybe Rational
  , gunHeading   :: Rational
  , radarHeading :: Rational
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

fromRawInput :: Rational -> Rational -> Raw.Input -> Input
fromRawInput g r rawInput = Input
  { life  = Raw.life  rawInput
  , speed = Raw.speed rawInput
  , enemy = Raw.enemy rawInput
  , wall  = Raw.wall  rawInput
  , gunHeading   = g
  , radarHeading = r
  }

toRawOutput :: Rational -> Rational -> s -> Output -> ((s,Rational,Rational),Raw.Output)
toRawOutput g r s output = (rawState, rawOutput)
  where
  rawState = ( s
             , g + gun   output * maxGunSpeed
             , r + radar output * maxRadarSpeed
             )
  rawOutput = Raw.Output
    { Raw.accel = accel output
    , Raw.body  = body  output
    , Raw.gun   = gun   output
    , Raw.radar = radar output
    , Raw.shoot = shoot output
    }

toRaw :: Tankode s -> Raw.Tankode (s,Rational,Rational)
toRaw tk  rin (s,g,r) = toRawOutput g r s' out
  where
  (s',out) = tk (fromRawInput g r rin) s

toRawIO :: TankodeIO s -> Raw.TankodeIO (s,Rational,Rational)
toRawIO tk  rin (s,g,r) = do
  (s',out) <- tk (fromRawInput g r rin) s
  return $ toRawOutput g r s' out

run :: Id -> Tankode s -> s -> IO ()
run id tk s0 = Raw.run id (toRaw tk) (s0,0,0)

runIO :: Id -> TankodeIO s -> s -> IO ()
runIO id tk s0 = Raw.runIO id (toRawIO tk) (s0,0,0)
