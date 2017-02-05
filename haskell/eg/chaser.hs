import Tankode.Basic

ident :: Id
ident = Id
  { name = "chaser"
  , trackColour  = "blue2"
  , bodyColour   = "blue4"
  , gunColour    = "grey7"
  , radarColour  = "grey1"
  , bulletColour = "grey9"
  , scanColour   = "blue1"
  }

chaser :: Tankode () -- :: Input () -> Output ()
chaser Input {enemy = Just d} = output
  { shoot = 1
  , accel = a
  }
  where
  a | d > 1     =  1
    | d < 1     = -1
    | otherwise =  0
chaser Input {enemy = Nothing, speed = s} = output
  { body = 1
  , accel = a
  }
  where
  a | s > 0     = -1
    | s < 0     =  1
    | otherwise =  0

main :: IO ()
main = run ident chaser ()
