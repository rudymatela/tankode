import Tankode.Raw

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

wal :: Tankode () -- :: Input -> () -> ((),Output)
wal Input {wall = Just d}
  | d > 1     = \_ -> ((), Output   1  0 0 0 1)
  | d < 1     = \_ -> ((), Output (-1) 0 0 0 1)
  | otherwise = \_ -> ((), Output   0  0 0 0 1)
wal Input {wall = Nothing, speed = s}
  | s > 0     = \_ -> ((), Output (-1) 1 0 0 0)
  | s < 0     = \_ -> ((), Output   1  1 0 0 0)
  | otherwise = \_ -> ((), Output   0  1 0 0 0)

main :: IO ()
main = run ident wal ()
