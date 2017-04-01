-- |
-- Program     : html-palette.hs
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Generate an HTML with the colour pallete for Tankode.
import Colour (Colour, showRGB)
import Colour (red,green,blue,cyan,magenta,yellow,black)
import Tankode.Palette
import Prelude hiding (head,break)
import Data.List (transpose)

break :: String -> String
break s = "\n" ++ s ++ "\n"

field0 :: String -> String -> String
field0 n = field n []

field :: String -> [String] -> String -> String
field n as "" = "<" ++ unwords (n:as) ++ " />"
field n as s  = "<" ++ unwords (n:as) ++ ">"
             ++ s
             ++ "</" ++ n ++ ">"

html :: String -> String
html = field0 "html" . break

head :: String -> String
head = field0 "head"

title :: String -> String
title = field0 "title"

body :: String -> String -> String
body bgcolor = field "body" ["bgcolor=" ++ bgcolor] . break

table :: String -> String
table = field "table" ["width=100%","height=100%","cellspacing=24"] . break

tr :: String -> String
tr = field0 "tr"

td :: String -> String
td bgcolor = field "td" ["bgcolor=" ++ bgcolor, "style=\"border-radius:6px\""] ""

main = putStrLn $ html $ h ++ "\n" ++ b
  where
  h = head . title $ "Color table"
  b = body (showRGB bgcolour) . table . init . unlines $ rs
  rs :: [String]
  rs = map (tr . concat) $ css
  css :: [[String]]
  css = map (map (td . showRGB)) colors

bgcolour :: Colour
bgcolour = Tankode.Palette.black


-- partial palette: all primary colors, some secondary and tertiary
colors :: [[Colour]]
colors = transpose
  [ [grey1,red1,orange1,yellow1,green1,blue1,magenta1,white1,black1]
  , [grey2,red2,orange2,yellow2,green2,blue2,magenta2,white2,black2]
  , [grey3,red3,orange3,yellow3,green3,blue3,magenta3,white3,black3]
  , [grey4,red4,orange4,yellow4,green4,blue4,magenta4,white4,black4]
  , [grey5,red5,orange5,yellow5,green5,blue5,magenta5,white5,black5]
  , [grey6,red6,orange6,yellow6,green6,blue6,magenta6,white6,black6]
  , [grey7,red7,orange7,yellow7,green7,blue7,magenta7,white7,black7]
  , [grey8,red8,orange8,yellow8,green8,blue8,magenta8,white8,black8]
  , [grey9,red9,orange9,yellow9,green9,blue9,magenta9,white9,black9]
  ]


{-
-- full palette: all primary, secondary and tertiary colors
colors :: [[Colour]]
colors = transpose
  [ [grey1,red1,orange1,yellow1,lime1,green1,aquamarine1,cyan1,azure1,blue1,violet1,magenta1,rose1]
  , [grey2,red2,orange2,yellow2,lime2,green2,aquamarine2,cyan2,azure2,blue2,violet2,magenta2,rose2]
  , [grey3,red3,orange3,yellow3,lime3,green3,aquamarine3,cyan3,azure3,blue3,violet3,magenta3,rose3]
  , [grey4,red4,orange4,yellow4,lime4,green4,aquamarine4,cyan4,azure4,blue4,violet4,magenta4,rose4]
  , [grey5,red5,orange5,yellow5,lime5,green5,aquamarine5,cyan5,azure5,blue5,violet5,magenta5,rose5]
  , [grey6,red6,orange6,yellow6,lime6,green6,aquamarine6,cyan6,azure6,blue6,violet6,magenta6,rose6]
  , [grey7,red7,orange7,yellow7,lime7,green7,aquamarine7,cyan7,azure7,blue7,violet7,magenta7,rose7]
  , [grey8,red8,orange8,yellow8,lime8,green8,aquamarine8,cyan8,azure8,blue8,violet8,magenta8,rose8]
  , [grey9,red9,orange9,yellow9,lime9,green9,aquamarine9,cyan9,azure9,blue9,violet9,magenta9,rose9]
  ]
-}
