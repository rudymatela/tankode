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
  { accel = 1
  , body  = 1
  , gun   = 1
  , shoot = case enemy input of
              Nothing -> 0
              Just _  -> 1/3
  }

main :: IO ()
main = run ident leftTurner ()
