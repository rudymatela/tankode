-- |
-- Module      : RatioMath
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Some mathematical functions over rationals.
module RatioMath
  ( module Data.Ratio
  , sin, cos, tan, asin, acos, atan, pi
  , sqrt
  , precision
  , roundR
  , (///)
  )
where

import Prelude hiding (sin, cos, tan, asin, acos, atan, pi, sqrt)
import qualified Prelude as P
import Data.Ratio
import Data.Maybe
import Test.LeanCheck.Error (errorToNothing)

(///) :: Rational -> Rational -> Maybe Rational
q /// r
  | r == 0    = Nothing
  | otherwise = Just $ q / r

tau :: Floating a => a
tau = P.pi * 2

precision :: Integral a => a
precision = 1080 -- maybe just 360 is enough

roundR :: Integer -> Rational -> Rational
roundR p r = (p * numerator r `div` denominator r) % p
-- TODO: actually use divRound above

rationalize :: RealFrac a => a -> Rational
rationalize x = round (x * fromIntegral precision) % precision

rationalize1 :: RealFrac a => (a -> a) -> Rational -> Rational
rationalize1 f = rationalize . f . fromRational

rationalize2 :: RealFrac a => (a -> a -> a) -> Rational -> Rational -> Rational
rationalize2 f = rationalize1 . f . fromRational

pi :: Rational
pi = rationalize P.pi

sin, cos, tan, asin, acos, atan :: Rational -> Rational
sin = rationalize1 $ P.sin . (tau *)
cos = rationalize1 $ P.cos . (tau *)
tan = rationalize1 $ P.tan . (tau *)
asin = rationalize1 $ (/ tau) . P.asin
acos = rationalize1 $ (/ tau) . P.acos
atan = (rationalize1 $ (/ tau) . P.atan) . fromError (fromIntegral precision)
-- the fromIntegral in atan allows us to atan (1%0).

sqrt :: Rational -> Rational
sqrt = rationalize1 P.sqrt

fromError :: a -> a -> a
fromError x = fromMaybe x . errorToNothing
