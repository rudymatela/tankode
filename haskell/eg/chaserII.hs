import Tankode.Basic

ident :: Id
ident = Id
  { name = "chaser"
  , trackColour  = "blue1"
  , bodyColour   = "blue3"
  , gunColour    = "grey7"
  , radarColour  = "grey1"
  , bulletColour = "grey9"
  , scanColour   = "blue1"
  }

data State = State
  { seenEnemy :: Bool
  , turnRight :: Bool
  }

chaserII :: Tankode State
chaserII Input {enemy = Just d, istate = state} = output
  { body = if turnRight state then -1 else 1
  , shoot = 1
  , accel = a
  , ostate = state {seenEnemy = True}
  }
  where
  a | d > 1     =  1
    | d < 1     = -1
    | otherwise =  0
chaserII Input {enemy = Nothing, speed = s, istate = state} = output
  { body = if t then -1 else 1
  , accel = a
  , ostate = state
      { seenEnemy = False
      , turnRight = t
      }
  }
  where
  t | seenEnemy state = not (turnRight state)
    | otherwise       = turnRight state
  a | s > 0     = -1
    | s < 0     =  1
    | otherwise =  0

iniState :: State
iniState = State
  { seenEnemy = False
  , turnRight = False
  }

main :: IO ()
main = run ident chaserII iniState
