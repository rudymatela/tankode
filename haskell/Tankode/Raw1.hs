module Tankode.Raw1
  ( Id (..), Colour
  , Input (..), Output (..), output
  , IncDec (..)
--, Tankode, TankodeIO
--, run, runIO
  , module Tankode.Constants
  )
where

import Tankode.Constants
import qualified Tankode.Raw0 as Raw0
import Tankode.Raw0 (IncDec(..), Id (..), Colour)

data Input s = Input
  { life   :: Rational
  , speed  :: Rational
  , enemy  :: Maybe Rational
  , wall   :: Maybe Rational
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
  , ostate = error "output: undefined output state"
  }

type Tankode s   = Input s ->     Output s
type TankodeIO s = Input s -> IO (Output s)

fromRaw0Input :: (s,Raw0.Input) -> Input s
fromRaw0Input = undefined

toRaw0Output :: Output s -> (s,Raw0.Output)
toRaw0Output = undefined

toRaw0 :: Tankode s -> Raw0.Tankode s
toRaw0 = undefined

toRaw0IO :: TankodeIO s -> Raw0.TankodeIO s
toRaw0IO = undefined

run :: Id -> Tankode s -> s -> IO ()
run = undefined

runIO :: Id -> TankodeIO s -> s -> IO ()
runIO = undefined
