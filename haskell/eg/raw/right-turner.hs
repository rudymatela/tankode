import Tankode.Raw

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

rightTurner :: Tankode () -- :: Input -> () -> ((),Output)
rightTurner Input {enemy = Just d} = \_ -> ((),Output 1 (-1) (-1) 0 (1/3))
rightTurner _                      = \_ -> ((),Output 1 (-1) (-1) 0 0)

main :: IO ()
main = run ident rightTurner ()
