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

toRawOutput :: Rational -> Rational -> s -> Output -> ((Rational,Rational,s),Raw.Output)
toRawOutput g r s output = undefined

-- R.Input -> s -> (s,gunHeading,radarHeading) -> IO ((s,gunHeading,radarHeading),R.Output)
-- toRaw :: Tankode s -> R.Tankode (s,Rational,Rational)
-- toRaw tk  rin (s,g,r) = (s',g',r',rout)
