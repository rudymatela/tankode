import Tankode.Raw

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

leftTurner :: Tankode () -- :: Input -> () -> ((),Output)
leftTurner Input {enemy = Just d} = \_ -> ((),Output 1 1 1 0 (1/3))
leftTurner _                      = \_ -> ((),Output 1 1 1 0 0)

main :: IO ()
main = run ident leftTurner ()
