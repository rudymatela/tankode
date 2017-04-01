-- |
-- Module      : Tankode.Raw1
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- A raw interface for implementing tankodes.
--
-- This interface imports "Tankode.Raw0" and changes it a bit by moving the
-- state into the 'Input' and 'Output' datatypes.
module Tankode.Raw1
  ( Id (..), Colour, IncDec (..)
  , Input (..), Output (..), output
  , Tankode, TankodeIO, run, runIO
  , module Tankode.Constants
  )
where

import Tankode.Constants
import qualified Tankode.Raw0 as Raw0
import Tankode.Common

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

fromRaw0Input :: s -> Raw0.Input -> Input s
fromRaw0Input s in0 = Input
  { life   = Raw0.life   in0
  , speed  = Raw0.speed  in0
  , enemy  = Raw0.enemy  in0
  , wall   = Raw0.wall   in0
  , istate = s
  }

toRaw0Output :: Output s -> (s,Raw0.Output)
toRaw0Output output = (ostate output,output0)
  where
  output0 = Raw0.Output
    { Raw0.accel = accel output
    , Raw0.body  = body  output
    , Raw0.gun   = gun   output
    , Raw0.radar = radar output
    , Raw0.shoot = shoot output
    }

toRaw0IO :: TankodeIO s -> Raw0.TankodeIO s
toRaw0IO tk in0 s = do
  output <- tk $ fromRaw0Input s in0
  return $ toRaw0Output output

run :: Id -> Tankode s -> s -> IO ()
run ident tk = runIO ident tkIO
  where
  tkIO i = return $ tk i

runIO :: Id -> TankodeIO s -> s -> IO ()
runIO id tk = Raw0.runIO id (toRaw0IO tk)
