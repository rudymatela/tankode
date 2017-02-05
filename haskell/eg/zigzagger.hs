import Tankode.Raw1
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

zigZagger :: Tankode Bool
zigZagger input = output
  { accel  = if back then (-1) else 1
  , shoot  = if isJust (enemy input) then 1 else 0
  , ostate = back
  }
  where
  back | speed input >=   1  = True
       | speed input <= (-1) = False
       | otherwise           = istate input

main :: IO ()
main = run ident zigZagger False
