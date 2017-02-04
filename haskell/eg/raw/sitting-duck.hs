import Tankode.Raw

ident :: Id
ident = Id
  { name = "sitting-duck"
  , bodyColour   = "yellow8"
  , trackColour  = "orange4"
  , gunColour    = "orange5"
  , bulletColour = "orange5"
  , radarColour  = "grey9"
  , scanColour   = "orange2"
  }

sittingDuck :: Tankode () -- :: Input -> () -> ((),Output)
sittingDuck input () = ((), Output 0 0 0 0 0)

main :: IO ()
main = run ident sittingDuck ()
