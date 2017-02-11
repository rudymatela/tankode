import Tankode.Basic

ident :: Id
ident = Id
  { name = "right-turner"
  , radarColour  = "grey9"
  , bodyColour   = "grey9"
  , trackColour  = "red4"
  , gunColour    = "blue4"
  , bulletColour = "blue8"
  , scanColour   = "blue8"
  }

rightTurner :: Tankode ()
rightTurner input = output
  { accel = 1
  , body  = -1
  , gun   = -maxGunTurn
  , shoot = case enemy input of
              Nothing -> 0
              Just _  -> 1/3
  }

main :: IO ()
main = run ident rightTurner ()
