-- |
-- |
-- Module      : Tankode.Palette.Utils
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Utilities to build colour palettes.
module Tankode.Palette.Utils (mkMaker, Colour, (%)) where

import Colour (fromHSL, fromHCL, Colour)
import Data.Ratio

project :: Rational -> (Rational,Rational) -> Rational
project r (r0,r1) | r0 < r1   =    r  * (r1-r0) + r0
                  | r0 > r1   = (1-r) * (r0-r1) + r1
                  | otherwise =                   r0

mkMaker :: Rational -> (Rational,Rational) -> (Rational,Rational)
        -> Rational -> Colour
mkMaker h sRange lRange  r =
  fromHCL (Just h) (project r sRange) (project r lRange)
