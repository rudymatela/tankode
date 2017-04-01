-- |
-- Module      : Tankode.Show
-- Copyright   : (c) 2016, 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Functions to read and show things from "Tankode.Data".
--
-- TODO: rename this module to ShowRead?
module Tankode.Show where

import Colour (Colour (..))
-- import Colour.OpenColor
import Tankode.Data
import Tankode.Palette
import Tankode.Physics
import Tankode.Constants
import Data.Ratio
import Data.Maybe
import Data.Char
import List
import Data.Monoid ((<>))
import RatioMath
import qualified Tankode.Obstacles as O

showField :: Field -> String
showField f = unwords
  [ "field"
  , showR $ width f
  , showR $ height f
  , showColour $ fieldColour f
  , showColour $ obstacleColour f
  ] ++ "\n" ++ init (unlines . map showObstacle $ obstacles f)

showObstacle :: Obstacle -> String
showObstacle ps = unwords $ "obstacle" : map showLoc ps

showId :: Tank -> String
showId t = unwords
  [ "tank"
  , name t
  , showColour $ trackColour  t
  , showColour $ bodyColour   t
  , showColour $ gunColour    t
  , showColour $ radarColour  t
  , showColour $ bulletColour t
  , showColour $ scanColour   t
  ]

showPosAndBullets :: Field -> State -> Tank -> String
showPosAndBullets f ts t = showPos f ts t ++ "\n" ++ showBullets (bullets t)

showPos :: Field -> State -> Tank -> String
showPos f ts t = unwords
  [ "tankpos"
  , showLoc $ loc t
  , showR $ heading   t
  , showR $ gun       t
  , showR $ radar     t
  , showR $ integrity t
  , showR $ power     t
  , showR $ heat      t
  , showR
  $ case scan t f ts of
      (Nothing,Nothing) -> scanRadius
      (Just  e,Nothing) -> e
      (Nothing,Just  w) -> w
      (Just  e,Just  w) -> min e w
  ]
  where
{- for debug:
        ++ "\n"
        ++ unlines
         [ unwords ["bullet", "1/12", showLoc l, "0/1"]
         | l <- radarProjection p
         ]
-}

showBullets :: [Bullet] -> String
showBullets = unlines . map showBullet

showBullet :: Bullet -> String
showBullet b@Bullet{bulletExploded = True} = unwords
  [ "explosion"
  , showR   (bulletCharge  b)
  , showLoc (bulletLoc     b)
  ]
showBullet b = unwords
  [ "bullet"
  , showR   (bulletCharge  b)
  , showLoc (bulletLoc     b)
  , showR   (bulletHeading b)
  ]

showColour :: Colour -> String
showColour (RGB r g b) = unwords $ map showR [r,g,b]

showLoc :: Loc -> String
showLoc (r,s) = showR r ++ " " ++ showR s

showR :: Rational -> String
showR r = show (numerator r) ++ "/" ++ show (denominator r)

showMR :: Maybe Rational -> String
showMR Nothing  = "-"
showMR (Just r) = show (numerator r) ++ "/" ++ show (denominator r)


showState :: Field -> State -> String
showState f ts = concatMap (showPosAndBullets f ts) ts

readR :: String -> Rational
readR = fromJust . readMR

readMR :: String -> Maybe Rational
readMR "-" = Nothing
readMR s = listToMaybe $ catMaybes [readMRFrac s, readMRDec s, readMRInt s]

readMRFrac :: String -> Maybe Rational
readMRFrac s =
  case span (/= '/') s of
    (n,'/':d) -> Just $ read n % read d
    _         -> Nothing

readMRDec :: String -> Maybe Rational
readMRDec s =
  case span (/= '.') s of
    ('-':n,'.':d) -> Just . negate $ readNumerator n + readDenominator d
    (n,'.':d)     -> Just          $ readNumerator n + readDenominator d
    _             -> Nothing
  where
  readNumerator   n = read n % 1
  readDenominator d = read d % 10 ^ fromIntegral (length d)

readMRInt :: String -> Maybe Rational
readMRInt s = Just $ read s % 1

readObstacle :: Rational -> Rational -> String -> [Obstacle]
readObstacle w h s =
  case trim ' ' s of
    "corners" -> O.corners w h
    "rounded" -> O.rounded w h
    "old"     -> O.old w h
    "center-circle"  -> O.centerCircle  w h
    "center-diamond" -> O.centerDiamond w h
    "center-square"  -> O.centerSquare  w h
    "ex"  -> O.ex  w h
    "ox"  -> O.ox  w h
    "bowtie" -> O.bowtie w h
    s -> (:[]) . pairwise . map readR . words $ gsub ',' ' ' s

readObstacles :: Rational -> Rational -> String -> [Obstacle]
readObstacles w h = concatMap (readObstacle w h) . split ';'

readColour :: String -> Colour
readColour ('#':n)     = fromInteger (read $ "0x" ++ n)
readColour ('0':'x':n) = fromInteger (read $ "0x" ++ n)
readColour "white"     = 0xffffff
readColour "black"     = 0x000000
readColour s | not (null s) && isDigit (last s) =
  mk $ digitToInt (last s)
  where
  mk = case init s of
         "grey"    -> mkGrey
         "red"     -> mkRed
         "green"   -> mkGreen
         "blue"    -> mkBlue
         "cyan"    -> mkCyan
         "magenta" -> mkMagenta
         "yellow"  -> mkYellow
         "orange"  -> mkOrange
readColour "grey"      = grey
readColour "red"       = red
readColour "green"     = green
readColour "blue"      = blue
readColour "cyan"      = cyan
readColour "magenta"   = magenta
readColour "yellow"    = yellow
readColour "orange"    = orange
readColour _           = error "readColour: no parse"
