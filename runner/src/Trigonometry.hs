-- |
-- Module      : Tankode
-- Copyright   : (c) 2016, 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Trigonometry functions for rational numbers.
--
-- I think this module is now unused in Tankode.
-- Check and remove soon.
module Trigonometry where

import Test.LeanCheck.Error (errorToNothing)
import Prelude hiding (pi, sin, cos, asin, acos, tan)
import qualified Prelude (atan, pi)
import Data.Ratio
import Data.List (find)
import Data.Maybe (fromMaybe)

mod1 :: Rational -> Rational
mod1 r = (numerator r `mod` denominator r) % denominator r

modulo :: Rational -> Rational -> Rational
n `modulo` d = mod1 (n / d) * d

pi :: Rational
pi = 47104 % 15015 -- piWith 8, approx 3.137

sin, cos, tan :: Rational -> Rational
sin = interpolate sins . mod1
cos = interpolate coss . mod1
tan th = sin th / cos th

-- asin range: -1%4 .. +1%4
-- acos range:    0 ..  1%2
asin, acos :: Rational -> Rational
asin = interpolate asins . (subtract 1) . (`modulo` 2) . (+1)
acos = interpolate acoss . (subtract 1) . (`modulo` 2) . (+1)

fromFloat :: Float -> Rational
fromFloat x = round (x * 1080) % 1080

fromError :: a -> a -> a
fromError x = fromMaybe x . errorToNothing

atan :: Rational -> Rational
atan r' = fromFloat $ Prelude.atan (fromRational r) / (2 * Prelude.pi)
  where
  r = fromError 1080 r'

sins :: [(Rational,Rational)]
sins = [ ( 0%12,  0  )
       , ( 1%12,  1%2)
    -- , ( 2%12,  7%10) -- add this approximation?
       , ( 3%12,  1  )
       , ( 5%12,  1%2)
       , ( 6%12,  0  )
       , ( 7%12, -1%2)
       , ( 9%12, -1  )
       , (11%12, -1%2)
       , (12%12,  0  )
       ]

asins :: [(Rational,Rational)]
asins = [ (-1  , -1%4 )
        , (-1%2, -1%12)
        , ( 0  ,  0   )
        , ( 1%2,  1%12)
        , ( 1  ,  1%4 )
        ]

coss :: [(Rational,Rational)]
coss = [ ( 0%12,  1  )
       , ( 2%12,  1%2)
       , ( 3%12,  0  )
       , ( 4%12, -1%2)
       , ( 6%12, -1  )
       , ( 8%12, -1%2)
       , ( 9%12,  0  )
       , (10%12,  1%2)
       , (12%12,  1  )
       ]

acoss :: [(Rational,Rational)]
acoss = [ (-1  ,  1%2 )
        , (-1%2,  4%12)
        , ( 0  ,  1%4 )
        , ( 1%2,  2%12)
        , ( 1  ,  0   )
        ]

crd :: Rational -> Rational
crd th = 2 * sin (th / 2)

acrd :: Rational -> Rational
acrd y = 2 * asin (y / 2)

interpolate :: [(Rational,Rational)] -> Rational -> Rational
interpolate qrs q | q == q0 = r0
                  | otherwise = (1 - delta) * r0 + delta * r1
  where
  delta = (q - q0) / (q1 - q0)
  Just ((q0,r0),(q1,r1)) = find (\((q0,_),(q1,_)) -> q0 <= q && q < q1)
                         . zip qrs $ tail qrs


piWith :: Int -> Rational
piWith = piNewton

-- John Wallis (1655)
piWallis :: Int -> Rational
piWallis n = (2 *) . product . take n $ zipWith (%) ns ds
  where
  ns = dups [2,4..]
  ds = 1 : dups [3,5..]
  dups (x:xs) = x:x:dups xs

-- Isaac Newton
piNewton :: Int -> Rational
piNewton n = 2 * p (toRational 1)
  where
  p m | m >= (toRational n) = 1
      | otherwise          = 1 + (m / (2*m + 1)) * p (m + 1)

sinWith :: Int -> Rational -> Rational
sinWith n x = sum $ map s [0..n]
  where
  s :: Int -> Rational
  s n = (-1)^n  *  x^(2*n + 1)
      / product [1..2 * toRational n + 1]

cosWith :: Int -> Rational -> Rational
cosWith n = undefined
