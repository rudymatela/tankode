import Tankode.Raw
import Data.Maybe

ident :: Id
ident = Id
  { name = "zig-zagger"
  , bodyColour   = "green1"
  , radarColour  = "green1"
  , trackColour  = "green2"
  , gunColour    = "magenta9"
  , bulletColour = "magenta9"
  , scanColour   = "magenta9"
  }

zigZagger :: Tankode Bool -- :: Input -> Bool -> (Bool,Output)
zigZagger input back' =
  ( back
  , output
      { accel = if back then (-1) else 1
      , shoot = if isJust (enemy input) then 1 else 0
      }
  )
  where
  back | speed input >=   1  = True
       | speed input <= (-1) = False
       | otherwise           = back'

main :: IO ()
main = run ident zigZagger False
