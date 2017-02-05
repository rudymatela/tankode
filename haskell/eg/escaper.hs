import Tankode.Basic

ident :: Id
ident = Id
  { name = "escaper"
  , bodyColour   = "grey2"
  , trackColour  = "grey4"
  , gunColour    = "grey6"
  , bulletColour = "grey6"
  , radarColour  = "grey1"
  , scanColour   = "grey5"
  }

escaper :: Tankode () -- :: Input () -> Output ()
escaper Input {enemy = Just d} = output
  { shoot = 1/4
  , accel = a
  }
  where
  a | d > 15    =  1
    | d < 15    = -1
    | otherwise =  0
escaper Input {enemy = Nothing, speed = s} = output
  { body = 1
  , accel = a
  }
  where
  a | s > 0     = -1
    | s < 0     =  1
    | otherwise =  0

main :: IO ()
main = run ident escaper ()
