-- |
-- Program     : html-palette.hs
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Prints the colour pallete of Tankode as a list of names and HEX colours.
import Colour (Colour, showRGB)
import Tankode.Palette

main = putStrLn . unlines $ map showColour colours
  where
  showColour (n,c) = n ++ " " ++ showRGB c

colours :: [(String,Colour)]
colours = 
  [ "black"   - black
  , "white"   - white
  , "grey"    - grey

  , "red"     - red
  , "green"   - green
  , "blue"    - blue

  , "cyan"    - cyan
  , "magenta" - magenta
  , "yellow"  - yellow

  , "orange"  - orange

  , "black1" - black1
  , "black2" - black2
  , "black3" - black3
  , "black4" - black4
  , "black5" - black5
  , "black6" - black6
  , "black7" - black7
  , "black8" - black8
  , "black9" - black9

  , "white1" - white1
  , "white2" - white2
  , "white3" - white3
  , "white4" - white4
  , "white5" - white5
  , "white6" - white6
  , "white7" - white7
  , "white8" - white8
  , "white9" - white9

  , "grey1" - grey1
  , "grey2" - grey2
  , "grey3" - grey3
  , "grey4" - grey4
  , "grey5" - grey5
  , "grey6" - grey6
  , "grey7" - grey7
  , "grey8" - grey8
  , "grey9" - grey9

  , "red1" - red1
  , "red2" - red2
  , "red3" - red3
  , "red4" - red4
  , "red5" - red5
  , "red6" - red6
  , "red7" - red7
  , "red8" - red8
  , "red9" - red9

  , "green1" - green1
  , "green2" - green2
  , "green3" - green3
  , "green4" - green4
  , "green5" - green5
  , "green6" - green6
  , "green7" - green7
  , "green8" - green8
  , "green9" - green9

  , "blue1" - blue1
  , "blue2" - blue2
  , "blue3" - blue3
  , "blue4" - blue4
  , "blue5" - blue5
  , "blue6" - blue6
  , "blue7" - blue7
  , "blue8" - blue8
  , "blue9" - blue9

  , "cyan1" - cyan1
  , "cyan2" - cyan2
  , "cyan3" - cyan3
  , "cyan4" - cyan4
  , "cyan5" - cyan5
  , "cyan6" - cyan6
  , "cyan7" - cyan7
  , "cyan8" - cyan8
  , "cyan9" - cyan9

  , "magenta1" - magenta1
  , "magenta2" - magenta2
  , "magenta3" - magenta3
  , "magenta4" - magenta4
  , "magenta5" - magenta5
  , "magenta6" - magenta6
  , "magenta7" - magenta7
  , "magenta8" - magenta8
  , "magenta9" - magenta9

  , "yellow1" - yellow1
  , "yellow2" - yellow2
  , "yellow3" - yellow3
  , "yellow4" - yellow4
  , "yellow5" - yellow5
  , "yellow6" - yellow6
  , "yellow7" - yellow7
  , "yellow8" - yellow8
  , "yellow9" - yellow9

  , "orange1" - orange1
  , "orange2" - orange2
  , "orange3" - orange3
  , "orange4" - orange4
  , "orange5" - orange5
  , "orange6" - orange6
  , "orange7" - orange7
  , "orange8" - orange8
  , "orange9" - orange9
  ]
  where (-) = (,)
