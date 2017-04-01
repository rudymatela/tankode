-- |
-- Module      : Tankode.Palette.Pure
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- A "pure" colour pallete.
module Tankode.Palette.Pure
  ( black, white, grey
  , red, green, blue
  , cyan, magenta, yellow
  , orange, lime, aquamarine, azure, violet, rose
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
  , orange1, lime1, aquamarine1, azure1, violet1, rose1
  , orange2, lime2, aquamarine2, azure2, violet2, rose2
  , orange3, lime3, aquamarine3, azure3, violet3, rose3
  , orange4, lime4, aquamarine4, azure4, violet4, rose4
  , orange5, lime5, aquamarine5, azure5, violet5, rose5
  , orange6, lime6, aquamarine6, azure6, violet6, rose6
  , orange7, lime7, aquamarine7, azure7, violet7, rose7
  , orange8, lime8, aquamarine8, azure8, violet8, rose8
  , orange9, lime9, aquamarine9, azure9, violet9, rose9

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

black, white, grey :: Colour
black = 0x000000
white = 0xffffff
grey  = 0x7f7f7f

red, green, blue :: Colour
red   = 0xff0000
green = 0x00ff00
blue  = 0x0000ff

cyan, magenta, yellow :: Colour
cyan    = 0x00ffff
magenta = 0xff00ff
yellow  = 0xffff00

orange, lime, aquamarine, azure, violet, rose :: Colour
orange     = 0xff7f00
lime       = 0x7fff00
aquamarine = 0x00ff7f
azure      = 0x007fff
violet     = 0x7f00ff
rose       = 0xff007f

mkMkr :: Rational -> (Rational,Rational) -> (Rational,Rational)
     -> Int -> Colour
mkMkr h sRange lRange  i = mkMaker h sRange lRange $ fromIntegral i % 10

pureMaker :: Rational -> Int -> Colour
pureMaker hue = mkMkr hue (1,1) (0,1)

mkBlack,mkGrey,mkWhite :: Int -> Colour
mkGrey    = mkMkr (  0%360) (0,0) (0,1)
mkBlack   = mkMkr (210%360) (0,0) (0,1/12)
mkWhite   = mkMkr (210%360) (0,0) (11/12,1)

mkRed, mkGreen, mkBlue, mkCyan, mkMagenta, mkYellow :: Int -> Colour
mkRed     = pureMaker (  0%360)
mkGreen   = pureMaker (120%360)
mkBlue    = pureMaker (240%360)
mkCyan    = pureMaker (180%360)
mkMagenta = pureMaker (300%360)
mkYellow  = pureMaker ( 60%360)

mkOrange, mkLime, mkAquamarine, mkAzure, mkViolet, mkRose :: Int -> Colour
mkOrange     = pureMaker ( 30%360)
mkLime       = pureMaker ( 90%360)
mkAquamarine = pureMaker (150%360)
mkAzure      = pureMaker (210%360)
mkViolet     = pureMaker (270%360)
mkRose       = pureMaker (330%360)

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

lime1, lime2, lime3, lime4, lime5, lime6, lime7, lime8, lime9 :: Colour
lime1 = mkLime 1
lime2 = mkLime 2
lime3 = mkLime 3
lime4 = mkLime 4
lime5 = mkLime 5
lime6 = mkLime 6
lime7 = mkLime 7
lime8 = mkLime 8
lime9 = mkLime 9

aquamarine1, aquamarine2, aquamarine3, aquamarine4, aquamarine5, aquamarine6, aquamarine7, aquamarine8, aquamarine9 :: Colour
aquamarine1 = mkAquamarine 1
aquamarine2 = mkAquamarine 2
aquamarine3 = mkAquamarine 3
aquamarine4 = mkAquamarine 4
aquamarine5 = mkAquamarine 5
aquamarine6 = mkAquamarine 6
aquamarine7 = mkAquamarine 7
aquamarine8 = mkAquamarine 8
aquamarine9 = mkAquamarine 9

azure1, azure2, azure3, azure4, azure5, azure6, azure7, azure8, azure9 :: Colour
azure1 = mkAzure 1
azure2 = mkAzure 2
azure3 = mkAzure 3
azure4 = mkAzure 4
azure5 = mkAzure 5
azure6 = mkAzure 6
azure7 = mkAzure 7
azure8 = mkAzure 8
azure9 = mkAzure 9

violet1, violet2, violet3, violet4, violet5, violet6, violet7, violet8, violet9 :: Colour
violet1 = mkViolet 1
violet2 = mkViolet 2
violet3 = mkViolet 3
violet4 = mkViolet 4
violet5 = mkViolet 5
violet6 = mkViolet 6
violet7 = mkViolet 7
violet8 = mkViolet 8
violet9 = mkViolet 9

rose1, rose2, rose3, rose4, rose5, rose6, rose7, rose8, rose9 :: Colour
rose1 = mkRose 1
rose2 = mkRose 2
rose3 = mkRose 3
rose4 = mkRose 4
rose5 = mkRose 5
rose6 = mkRose 6
rose7 = mkRose 7
rose8 = mkRose 8
rose9 = mkRose 9
