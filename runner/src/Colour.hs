-- |
-- Module      : Colour
-- Copyright   : (c) 2016-2017  Rudy Matela
-- License     : 3-Clause BSD
--
-- Simple colour module.  (Distributed as part of Tankode.)
--
-- The initial version of this module was created by Rudy on Dec 2016.
module Colour
  ( Colour (RGB)
  , Color
  , showRGB
  , (.+.), (.-.), (.*.)
  , black, white
  , red, green, blue
  , cyan, magenta, yellow
  , orange
  , grey
  , makeGrey
  , grey1, grey2, grey3, grey4, grey5, grey6, grey7, grey8, grey9
  , rgb, cmy
  , chroma
  , hue
  , intensity, value, lightness
  , saturation, saturationHSV, saturationHSL, saturationHSI
  , fromRGB, fromCMY, fromHSV, fromHSL, fromHCL, fromHCM
  , mix, mixHSV
  , primary

  -- * Misc Utils
  , frac
  , coerceRatio
  )
where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Tuple
import Data.Functor ((<$>)) -- for GHC < 7.10
import Control.Applicative ((<*>)) -- for GHC < 7.10

data Colour = RGB Rational Rational Rational
  deriving (Eq, Ord)

type Color = Colour

instance Show Colour where
  show c@(RGB r g b) = "RGB (" ++ show r ++ ") (" ++ show g ++ ") (" ++ show b ++ ")"
          ++ " {- " ++ showRGB c ++ " -}"

showRGB :: Colour -> String
showRGB (RGB r g b) = "#" ++ hexRatio r ++ hexRatio g ++ hexRatio b

hexRatio :: Integral a => Ratio a -> String
hexRatio r = hex $ numerator r * 0xFF `div` denominator r

hex :: Integral a => a -> String
hex = (\s -> case s of
               []  -> "00"
               [c] -> '0':[c]
               cs  -> cs)
    . map (intToDigit . coerceNum)
    . reverse
    . unfoldr (\n -> listToMaybe [swap $ n `divMod` 16 | n /= 0])

coerceNum :: (Integral a, Num b) => a -> b
coerceNum = fromInteger . toInteger

coerceRatio :: (Integral a, Integral b) => Ratio a -> Ratio b
coerceRatio r = coerceNum (numerator r) % coerceNum (denominator r)

mod1 :: Integral a => Ratio a -> Ratio a
mod1 r = (numerator r `mod` denominator r) % denominator r

modulo :: Integral a => Ratio a -> Ratio a -> Ratio a
n `modulo` d = mod1 (n / d) * d

frac :: Integral a => Ratio a -> Ratio a
frac r | r < 0 = 0
       | r > 1 = 1
       | otherwise = r

instance Num Colour where
  RGB r1 g1 b1 + RGB r2 g2 b2 = RGB (frac $ r1 + r2) (frac $ g1 + g2) (frac $ b1 + b2)
  RGB r1 g1 b1 - RGB r2 g2 b2 = RGB (frac $ r1 - r2) (frac $ g1 - g2) (frac $ b1 - b2)
  RGB r1 g1 b1 * RGB r2 g2 b2 = RGB        (r1 * r2)        (g1 * g2)        (b1 * b2)
  negate (RGB r g b) = RGB (1 - r) (1 - g) (1 - b)
  abs c = c
  signum c = 1
  fromInteger i = let j = i `div` 0x100
                      k = j `div` 0x100
                  in RGB (k `mod` 0x100 % 255) (j `mod` 0x100 % 255) (i `mod` 0x100 % 255)

(.+.) :: Colour -> Colour -> Colour
c1 .+. c2 = negate $ negate c1 + negate c2

(.-.) :: Colour -> Colour -> Colour
c1 .-. c2 = negate $ negate c1 - negate c2

(.*.) :: Colour -> Colour -> Colour
c1 .*. c2 = negate $ negate c1 * negate c2

black :: Colour
black = RGB 0 0 0

white :: Colour
white = RGB 1 1 1

red :: Colour
red = RGB 1 0 0

green :: Colour
green = RGB 0 1 0

blue :: Colour
blue = RGB 0 0 1

cyan :: Colour
cyan = RGB 0 1 1

magenta :: Colour
magenta = RGB 1 0 1

yellow :: Colour
yellow = RGB 1 1 0

orange :: Colour
orange = RGB 1 (1%2) 0

grey :: Colour
grey = grey5

grey1, grey2, grey3, grey4, grey5, grey6, grey7, grey8, grey9 :: Colour
grey1 = makeGrey $ 1%10
grey2 = makeGrey $ 2%10
grey3 = makeGrey $ 3%10
grey4 = makeGrey $ 4%10
grey5 = makeGrey $ 5%10
grey6 = makeGrey $ 6%10
grey7 = makeGrey $ 7%10
grey8 = makeGrey $ 8%10
grey9 = makeGrey $ 9%10

makeGrey :: Rational -> Colour
makeGrey r = RGB r r r

rgb :: Colour -> (Rational, Rational, Rational)
rgb (RGB r g b) = (r,g,b)

cmy :: Colour -> (Rational, Rational, Rational)
cmy (RGB r g b) = (1 - r, 1 - g, 1 - b)

maxi :: Colour -> Rational
maxi (RGB r g b) = maximum [r,g,b]

mini :: Colour -> Rational
mini (RGB r g b) = minimum [r,g,b]

chroma :: Colour -> Rational
chroma c = maxi c - mini c

hue :: Colour -> Maybe Rational
hue color@(RGB r g b) = (\h' -> mod1 $ h' / 6) <$> h' -- * 60 / 360
  where
  c = chroma color
  m = maxi color
  h' | c == 0 = Nothing
     | m == r = Just $ (g - b) / c
     | m == g = Just $ (b - r) / c + 2
     | m == b = Just $ (r - g) / c + 4

intensity :: Colour -> Rational
intensity (RGB r g b) = (r + g + b) / 3

value :: Colour -> Rational
value c = maxi c

lightness :: Colour -> Rational
lightness c = (maxi c + mini c) / 2

saturation :: Colour -> Rational
saturation = saturationHSV

saturationHSV :: Colour -> Rational
saturationHSV c =
  if value c == 0
    then 0
    else chroma c / value c

saturationHSL :: Colour -> Rational
saturationHSL c =
  if lightness c == 1
    then 0
    else chroma c / (1 - abs (2 * lightness c - 1))

saturationHSI :: Colour -> Rational
saturationHSI c =
  case intensity c of
    0 -> 0
    i -> 1 - mini c/i

fromRGB :: Rational -> Rational -> Rational -> Colour
fromRGB = RGB

-- TODO: double check this, I don't think this is quite right
fromCMY :: Rational -> Rational -> Rational -> Colour
fromCMY c m y = RGB (1 - c) (1 - m) (1 - y)

fromHSV :: Maybe Rational -> Rational -> Rational -> Colour
fromHSV h s v = fromHCM h c m
  where
  c = v * s
  m = v - c

fromHSL :: Maybe Rational -> Rational -> Rational -> Colour
fromHSL h s l = fromHCM h c m
  where
  c = (1 - abs (2*l - 1)) * s
  m = l - c / 2

fromHCL :: Maybe Rational -> Rational -> Rational -> Colour
fromHCL h c l = fromHCM h c m  where m = (1 - c) * l

-- | From hue, chroma and min
fromHCM :: Maybe Rational -> Rational -> Rational -> Colour
fromHCM h c m = RGB (r' + m) (g' + m) (b' + m)
  where
  h' = case h of
         Nothing -> 0
         Just h' -> mod1 h'
  x = c * (1 - abs ((h'*6) `modulo` 2 - 1))
  (r',g',b')
    | 0%6 <= h' && h' <= 1%6 = (c,x,0)
    | 1%6 <= h' && h' <= 2%6 = (x,c,0)
    | 2%6 <= h' && h' <= 3%6 = (0,c,x)
    | 3%6 <= h' && h' <= 4%6 = (0,x,c)
    | 4%6 <= h' && h' <= 5%6 = (x,0,c)
    | 5%6 <= h' && h' <= 6%6 = (c,0,x)

mix :: Colour -> Colour -> Colour
mix (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2)

mixHSV :: Colour -> Colour -> Colour
mixHSV c1 c2 = fromHSV (mod1 <$> ((+) <$> hue c1 <*> hue c2))
                       ((saturationHSV c1 + saturationHSV c2) / 2)
                       ((value c1 + value c2) / 2)

primary :: Colour -> Bool
primary c = c == red
         || c == green
         || c == blue
