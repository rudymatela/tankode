import Tankode.Basic
import Data.Maybe (isJust)
import Debug.Trace

ident :: Id
ident = Id
  { name = "chaser"
  , trackColour  = "magenta1"
  , bodyColour   = "magenta3"
  , gunColour    = "grey7"
  , radarColour  = "grey1"
  , bulletColour = "grey9"
  , scanColour   = "magenta1"
  }

data State = State
  { seenEnemy :: Bool
  , turnRight :: Bool
  , scanRight :: Bool
  , acc :: IncDec
  }

iniState :: State
iniState = State
  { seenEnemy = False
  , turnRight = False
  , scanRight = False
  , acc = 0
  }

maxRadarHeading = 2 * maxRadarTurn

computeRadar :: Input State -> Rational
computeRadar Input{radarHeading = r, istate = State{scanRight = s}}
  | r ==   maxRadarHeading  = -maxRadarTurn
  | r == (-maxRadarHeading) =  maxRadarTurn
  | s                       = -maxRadarTurn
  | otherwise               = maxRadarTurn

scanner :: Tankode State
scanner input@Input{radarHeading = r, istate = state} = output
  { radar = computeRadar input
  , accel = acc state
  , body = if turnRight state
             then -1
             else 1
  , ostate = state
      { turnRight =
          if not (seenEnemy state) && isJust (enemy input)
            then radarHeading input < 0
            else turnRight state
      , scanRight = sr
      }
  }
  where
  sr | r ==   maxRadarHeading  = True
     | r == (-maxRadarHeading) = False
     | otherwise               = scanRight state

shooter :: Tankode State
shooter input@Input{enemy = Just d, istate = state} = output
  { body = if turnRight state then -1 else 1
  , shoot = 1
  , accel = a
  , radar = computeRadar input
  , ostate = state
      { seenEnemy = True
      , acc = a
      }
  }
  where
  a | d > 1     =  1
    | d < 1     = -1
    | otherwise =  0
shooter input@Input{enemy = Nothing, speed = s, istate = state} = output
  { body = if t then -1 else 1
  , accel = a
  , radar = computeRadar input
  , ostate = state
      { seenEnemy = False
      , turnRight = t
      , acc = a
      }
  }
  where
  t | seenEnemy state = not (turnRight state)
    | otherwise       = turnRight state
  a | s > 0     = -1
    | s < 0     =  1
    | otherwise =  0

chaserIII :: Tankode State
chaserIII input
  | radarHeading input == 0 = shooter input
  | otherwise               = scanner input

main :: IO ()
main = run ident chaserIII iniState
