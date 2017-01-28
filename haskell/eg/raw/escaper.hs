import Tankode.Raw

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

escaper :: Tankode () -- :: Input -> () -> ((),Output)
escaper Input {enemy = Just d}
  | d > 15    = \_ -> ((), Output   1  0 0 0 (1/4))
  | d < 15    = \_ -> ((), Output (-1) 0 0 0 (1/4))
  | otherwise = \_ -> ((), Output   0  0 0 0 (1/4))
escaper Input {enemy = Nothing, speed = s}
  | s > 0     = \_ -> ((), Output (-1) 1 0 0 0)
  | s < 0     = \_ -> ((), Output   1  1 0 0 0)
  | otherwise = \_ -> ((), Output   0  1 0 0 0)

main :: IO ()
main = run ident escaper ()
