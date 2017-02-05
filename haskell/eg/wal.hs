import Tankode.Basic

ident :: Id
ident = Id
  { name = "wal"
  , trackColour  = "grey3"
  , bodyColour   = "grey4"
  , gunColour    = "grey9"
  , radarColour  = "grey4"
  , bulletColour = "grey4"
  , scanColour   = "grey4"
  }

wal :: Tankode ()
wal Input {wall = Just d} = output
  { shoot = 1
  , accel = a
  }
  where
  a | d > 1     =  1
    | d < 1     = -1
    | otherwise =  0
wal Input {wall = Nothing, speed = s} = output
  { body = 1
  , accel = a
  }
  where
  a | s > 0     = -1
    | s < 0     =  1
    | otherwise =  0

main :: IO ()
main = run ident wal ()
