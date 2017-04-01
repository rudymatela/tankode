-- |
-- Module      : Tankode.Palette.Year2017
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- A pentadic colour palette.
--
-- How this palette is (roughly) built:
-- * The 5 initial colour hues are spread in 5 equidistant directions on the
--   colour wheel starting with an azureish blue at 205 degrees then going
--   to magenta, red, yellow and green.
-- * the orange hue is added as the median of yellow and red
-- * the cyan hue is added as the median of green and blue
-- * chroma is kept contant at 50%
-- * each hue has 9 colors of varying lightness.
--
-- For more details:
--   * read the code  :-)
--   * reverse engineer from resulting values
--   * ask me (Rudy)
module Tankode.Palette.Year2017
  ( black, white, grey, red, green, blue, cyan, magenta, yellow, orange
  , darkGrey, darkRed, darkGreen, darkBlue, darkCyan, darkMagenta, darkYellow, darkOrange
  , lightGrey, lightRed, lightGreen, lightBlue, lightCyan, lightMagenta, lightYellow, lightOrange
-- Greyscale

  , black1, white1, grey1
  , black2, white2, grey2
  , black3, white3, grey3
  , black4, white4, grey4
  , black5, white5, grey5
  , black6, white6, grey6
  , black7, white7, grey7
  , black8, white8, grey8
  , black9, white9, grey9

-- Primary colors
  , red1, green1, blue1
  , red2, green2, blue2
  , red3, green3, blue3
  , red4, green4, blue4
  , red5, green5, blue5
  , red6, green6, blue6
  , red7, green7, blue7
  , red8, green8, blue8
  , red9, green9, blue9

-- Secondary colors
  , cyan1, magenta1, yellow1
  , cyan2, magenta2, yellow2
  , cyan3, magenta3, yellow3
  , cyan4, magenta4, yellow4
  , cyan5, magenta5, yellow5
  , cyan6, magenta6, yellow6
  , cyan7, magenta7, yellow7
  , cyan8, magenta8, yellow8
  , cyan9, magenta9, yellow9

-- Tertiary colors
  , orange1
  , orange2
  , orange3
  , orange4
  , orange5
  , orange6
  , orange7
  , orange8
  , orange9

-- * Color makers
  , mkGrey
  , mkRed
  , mkGreen
  , mkBlue
  , mkCyan
  , mkMagenta
  , mkYellow
  , mkOrange
  )
where

import Tankode.Palette.Utils

black, white, grey, red, green, blue, cyan, magenta, yellow, orange :: Colour
black   = black5
white   = white5
grey    = grey5
red     = red5
green   = green5
blue    = blue5
cyan    = cyan5
magenta = magenta5
yellow  = yellow5
orange  = orange5

darkGrey :: Colour
darkGrey = grey3

darkRed, darkGreen, darkBlue :: Colour
darkRed   = red3
darkGreen = green3
darkBlue  = blue3

darkCyan, darkMagenta, darkYellow :: Colour
darkCyan    = cyan3
darkMagenta = magenta3
darkYellow  = yellow3

darkOrange :: Colour
darkOrange = orange3

lightGrey :: Colour
lightGrey = grey7

lightRed, lightGreen, lightBlue :: Colour
lightRed   = red7
lightGreen = green7
lightBlue  = blue7

lightCyan, lightMagenta, lightYellow :: Colour
lightCyan    = cyan7
lightMagenta = magenta7
lightYellow  = yellow7

lightOrange :: Colour
lightOrange = orange7

blueHue :: Rational
blueHue = 210%360 - 5%360

mkMkr :: Rational -> (Rational,Rational) -> (Rational,Rational)
     -> Int -> Colour
mkMkr h sRange lRange  i = mkMaker h sRange lRange $ fromIntegral (i-1) % 8 

yearMaker :: Rational -> Int -> Colour
yearMaker hue = mkMkr hue (30%60,30%60) ( 0%60,60%60)

mkBlack,mkGrey,mkWhite :: Int -> Colour
mkGrey    = mkMkr (210%360) ( 3%60, 3%60) (12%60,57%60)
mkBlack   = mkMkr (210%360) ( 3%60, 3%60) ( 0%60, 3%60)
mkWhite   = mkMkr (210%360) ( 3%60, 3%60) (57%60,60%60)

mkRed, mkGreen, mkBlue, mkCyan, mkMagenta, mkYellow, mkOrange :: Int -> Colour
mkOrange  = yearMaker (blueHue + 1%2)
mkBlue    = yearMaker (blueHue      )
mkMagenta = yearMaker (blueHue + 1%5)
mkRed     = yearMaker (blueHue + 2%5)
mkYellow  = yearMaker (blueHue + 3%5)
mkGreen   = yearMaker (blueHue + 4%5)
mkCyan    = yearMaker (blueHue + 9%10)

black1, black2, black3, black4, black5, black6, black7, black8, black9 :: Colour
black1 = mkBlack 1
black2 = mkBlack 2
black3 = mkBlack 3
black4 = mkBlack 4
black5 = mkBlack 5
black6 = mkBlack 6
black7 = mkBlack 7
black8 = mkBlack 8
black9 = mkBlack 9

white1, white2, white3, white4, white5, white6, white7, white8, white9 :: Colour
white1 = mkWhite 1
white2 = mkWhite 2
white3 = mkWhite 3
white4 = mkWhite 4
white5 = mkWhite 5
white6 = mkWhite 6
white7 = mkWhite 7
white8 = mkWhite 8
white9 = mkWhite 9

grey1, grey2, grey3, grey4, grey5, grey6, grey7, grey8, grey9 :: Colour
grey1 = mkGrey 1
grey2 = mkGrey 2
grey3 = mkGrey 3
grey4 = mkGrey 4
grey5 = mkGrey 5
grey6 = mkGrey 6
grey7 = mkGrey 7
grey8 = mkGrey 8
grey9 = mkGrey 9

red1, red2, red3, red4, red5, red6, red7, red8, red9 :: Colour
red1 = mkRed 1
red2 = mkRed 2
red3 = mkRed 3
red4 = mkRed 4
red5 = mkRed 5
red6 = mkRed 6
red7 = mkRed 7
red8 = mkRed 8
red9 = mkRed 9

green1, green2, green3, green4, green5, green6, green7, green8, green9 :: Colour
green1 = mkGreen 1
green2 = mkGreen 2
green3 = mkGreen 3
green4 = mkGreen 4
green5 = mkGreen 5
green6 = mkGreen 6
green7 = mkGreen 7
green8 = mkGreen 8
green9 = mkGreen 9

blue1, blue2, blue3, blue4, blue5, blue6, blue7, blue8, blue9 :: Colour
blue1 = mkBlue 1
blue2 = mkBlue 2
blue3 = mkBlue 3
blue4 = mkBlue 4
blue5 = mkBlue 5
blue6 = mkBlue 6
blue7 = mkBlue 7
blue8 = mkBlue 8
blue9 = mkBlue 9

cyan1, cyan2, cyan3, cyan4, cyan5, cyan6, cyan7, cyan8, cyan9 :: Colour
cyan1 = mkCyan 1
cyan2 = mkCyan 2
cyan3 = mkCyan 3
cyan4 = mkCyan 4
cyan5 = mkCyan 5
cyan6 = mkCyan 6
cyan7 = mkCyan 7
cyan8 = mkCyan 8
cyan9 = mkCyan 9

magenta1, magenta2, magenta3, magenta4, magenta5, magenta6, magenta7, magenta8, magenta9 :: Colour
magenta1 = mkMagenta 1
magenta2 = mkMagenta 2
magenta3 = mkMagenta 3
magenta4 = mkMagenta 4
magenta5 = mkMagenta 5
magenta6 = mkMagenta 6
magenta7 = mkMagenta 7
magenta8 = mkMagenta 8
magenta9 = mkMagenta 9

yellow1, yellow2, yellow3, yellow4, yellow5, yellow6, yellow7, yellow8, yellow9 :: Colour
yellow1 = mkYellow 1
yellow2 = mkYellow 2
yellow3 = mkYellow 3
yellow4 = mkYellow 4
yellow5 = mkYellow 5
yellow6 = mkYellow 6
yellow7 = mkYellow 7
yellow8 = mkYellow 8
yellow9 = mkYellow 9

orange1, orange2, orange3, orange4, orange5, orange6, orange7, orange8, orange9 :: Colour
orange1 = mkOrange 1
orange2 = mkOrange 2
orange3 = mkOrange 3
orange4 = mkOrange 4
orange5 = mkOrange 5
orange6 = mkOrange 6
orange7 = mkOrange 7
orange8 = mkOrange 8
orange9 = mkOrange 9
