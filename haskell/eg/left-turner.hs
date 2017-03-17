import Tankode.Basic

ident :: Id
ident = Id
  { name = "left-turner"
  , bodyColour   = "red3"
  , radarColour  = "red3"
  , scanColour   = "red8"
  , trackColour  = "red3"
  , bulletColour = "red9"
  , gunColour    = "yellow7"
  }

leftTurner :: Tankode ()
leftTurner input = output
  { accel = 0
  , body  = 0
  , gun   = maxGunTurn
  , shoot = case enemy input of
              Nothing -> 0
              Just _  -> 2/3
  }

main :: IO ()
main = run ident leftTurner ()
