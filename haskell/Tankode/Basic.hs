module Tankode.Basic
  ( Id (..)
  , module Tankode.Constants
  )
where

import Data.Ratio
import Data.Maybe
import Control.Monad
import System.IO
import qualified Tankode.Raw as R
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
