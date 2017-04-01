-- |
-- Module      : Geometry
-- Copyright   : (c) 2016, 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- A simple geometry module.
--
-- Note some names are currently (2017-01) inconsistent with function
-- behaviour. e.g.: intersectCS will return true for a segment inside a circle
-- (not actually checking for intersection with the circle, but with the area
-- inside it).
module Geometry
  (
-- * datatypes
    Point
  , Vector
  , Segment
  , Box
  , Line
  , Circle

-- * validness
  , validL

-- * coincidence
  , coincideL
  , distinctL

-- * lines and points
  , collinearity
  , lineFromPoints
  , lineFromSegment
  , linePoint
  , midpoint

-- * intersection
  , intersectSS
  , intersectCS
  , intersectCC
  , intersectBB
  , insideBox
  , insideTriangle
  , intersectionPointLL
  , intersectionPointSS
  , parallel

-- * distance
  , distancePP
  , sqDistancePP
  , sqDistanceLP
  , sqDistanceSP
  , sqDistanceTP
  , sqDistancePolyPoint

-- * rotation
  , rotatePoint
  , rotateLine
  , rotateLine0

-- * translation
  , translateLine
  , translatePoint

-- * circles
  , secantPoints
  , secantPoints0
  , secantPointsS

-- * bounding boxes
  , bboxS
  , bboxC
  , grow

-- * segments
  , polygonSegments
  )
where

import Prelude hiding (sqrt)
import RatioMath
import Data.Maybe (isNothing)

type Point    = (Rational,Rational)
type Vector   = Point
type Segment  = (Point,Point)
type Box      = (Point,Point)
type Line     = (Rational,Rational,Rational) -- ax + by + c = 0
type Triangle = (Point,Point,Point)
type Polygon  = [Point]
type Circle   = (Point,Rational)

-- Checks if a line is valid:
-- does not consider the "plane" a line (0,0,0);
-- does not consider "nothing" a line (0,0,1).
validL :: Line -> Bool
validL (0,0,x) = False
validL _       = True

-- This is HARD!
coincideL :: Line -> Line -> Bool
coincideL (a1,b1,c1) (a2,b2,c2) = a == b && b == c
  where
  a = a1 /// a2
  b = b1 /// b2
  c = c1 /// c2
-- TODO: this is wrong, fix!

distinctL :: Line -> Line -> Bool
distinctL = not .: coincideL

-- collinearity using homogeneous coordinates
collinearity :: Point -> Point -> Point -> Rational
collinearity (ax,ay) (bx,by) (cx,cy) =
    aw * bx * cy
  + ax * by * cw
  + ay * bw * cx
  - cw * bx * ay
  - cx * by * aw
  - cy * bw * ax
  where
  (aw,bw,cw) = (1,1,1)

lineFromPoints :: Point -> Point -> Line
lineFromPoints (x1,y1) (x2,y2) = (y1 - y2, x2 - x1, (x1-x2)*y1 - (y1-y2)*x1)

lineFromSegment :: Segment -> Line
lineFromSegment (p1,p2) = lineFromPoints p1 p2

-- apply a point to a line
linePoint :: Line -> Point -> Rational
linePoint (a,b,c) (x,y) = a*x + b*y + c

midpoint :: Point -> Point -> Point
midpoint (x1,y1) (x2,y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

incidentL :: Point -> Line -> Bool
incidentL p l = linePoint l p == 0

incidentS :: Point -> Segment -> Bool
incidentS p s = incidentL p (lineFromSegment s)
             && insideBox p (bboxS s)

-- | computes wether two bounding boxes intersect (this includes internal
--   points)
intersectBB :: Box -> Box -> Bool
intersectBB ((a0x,a0y),(a1x,a1y)) ((b0x,b0y),(b1x,b1y)) =
     a0x <= b1x
  && a1x >= b0x
  && a0y <= b1y
  && a1y >= b0y

-- sounds hacky, but very mathematical
intersectSS :: Segment -> Segment -> Bool
intersectSS a@(a1,a2) b@(b1,b2) =
     intersectBB (bboxS a) (bboxS b)
  && 0 `between` (ab1,ab2)
  && 0 `between` (ba1,ba2)
  where
  ab1 = collinearity a1 a2 b1
  ab2 = collinearity a1 a2 b2
  ba1 = collinearity b1 b2 a1
  ba2 = collinearity b1 b2 a2

intersectCS :: Circle -> Segment -> Bool
intersectCS c@(c0,r) s = intersectBB (bboxS s) (bboxC c)
                      && sqDistanceSP s c0 <= r*r

intersectCC :: Circle -> Circle -> Bool
intersectCC c1@(cc1,r1) c2@(cc2,r2) =
     intersectBB (bboxC c1) (bboxC c2)
  && sqDistancePP cc1 cc2 <= (r1+r2)^2

insideTriangle :: Triangle -> Point -> Bool
insideTriangle (a,b,c) = insidePolygon [a,b,c]

insidePolygon :: Polygon -> Point -> Bool
insidePolygon poly p = all (>= 0) cs
                    || all (<= 0) cs
  where
  cs = map (\(a,b) -> collinearity a b p)
     $ polygonSegments poly

insideBox :: Point -> Box -> Bool
insideBox (x,y) ((x0,y0),(x1,y1)) = x `between` (x0,x1)
                                 && y `between` (y0,y1)

parallel :: Line -> Line -> Bool
parallel = isNothing .: intersectionPointLL

-- for parallel lines, returns Nothing
intersectionPointLL :: Line -> Line -> Maybe Point
intersectionPointLL l1@(a1,b1,c1) l2@(a2,b2,c2) =
  if w == 0
    then Nothing
    else Just (x/w,y/w)
  where
  x = b1*c2 - b2*c1
  y = a2*c1 - a1*c2
  w = a1*b2 - a2*b1

-- for parallel segments, returns Nothing
intersectionPointSS :: Segment -> Segment -> Maybe Point
intersectionPointSS s1 s2 =
  if intersectBB b1 b2
    then
      case intersectionPointLL (lineFromSegment s1) (lineFromSegment s2) of
        Nothing -> Nothing
        Just p ->
          if p `insideBox` b1 && p `insideBox` b2
            then Just p
            else Nothing
    else Nothing
  where
  b1 = bboxS s1
  b2 = bboxS s2

-- as defined here: http://mathworld.wolfram.com/Circle-LineIntersection.html
-- this returns the secant points of the line defined by the segment, not the segment itself
secantPoints0 :: Segment -> Rational -> Maybe (Point, Point)
secantPoints0 ((x1,y1),(x2,y2)) r =
  if delta <= 0
    then Nothing
    else Just ((x,y),(x',y'))
  where
  dx = x2 - x1
  dy = y2 - y1
  drdr = dx^2 + dy^2
  d = x1*y2 - x2*y1 -- big d
  delta = r*r * drdr - d*d
  x  = ( d * dy + sign dy * dx * sqrt delta) / drdr
  x' = ( d * dy - sign dy * dx * sqrt delta) / drdr
  y  = (-d * dx +       abs dy * sqrt delta) / drdr
  y' = (-d * dx -       abs dy * sqrt delta) / drdr
  sign x | x < 0     = -1
         | otherwise = 1

-- secant points of a line defined by a segment
secantPoints :: Segment -> Circle -> Maybe (Point, Point)
secantPoints ((x1,y1),(x2,y2)) ((x,y),r) =
  case secantPoints0 ((x1-x,y1-y),(x2-x,y2-y)) r of
    Nothing -> Nothing
    Just ((x1,y1),(x2,y2)) -> Just ((x1+x,y1+y),(x2+x,y2+y))

-- actual secant points of a segment
secantPointsS :: Segment -> Circle -> [Point]
secantPointsS s c = filter (`insideBox` bboxS s) . foo $ secantPoints s c
  where
  foo Nothing      = []
  foo (Just (a,b)) = [a,b]


distancePP :: Point -> Point -> Rational
distancePP = RatioMath.sqrt .: sqDistancePP

sqDistancePP :: Point -> Point -> Rational
sqDistancePP (x1,y1) (x2,y2) = (x2-x1)^2 + (y2-y1)^2

sqDistanceLP :: Line -> Point -> Rational
sqDistanceLP l@(a,b,c) p = (linePoint l p)^2 / (a*a + b*b)

sqDistanceSP :: Segment -> Point -> Rational
sqDistanceSP (p1,p2) p =
  if signum (collinearity p1 p2' p)
  /= signum (collinearity p1' p2 p)
    then sqDistanceLP l p
    else sqDistancePP p1 p `min` sqDistancePP p2 p
  where
  l = lineFromPoints p1 p2
  p1' = rotatePoint90 p2 p1
  p2' = rotatePoint90 p1 p2
  rotatePoint90 = rotatePoint 1 0

sqDistanceTP :: Triangle -> Point -> Rational
sqDistanceTP (a,b,c) = sqDistancePolyPoint [a,b,c]

sqDistancePolyPoint :: Polygon -> Point -> Rational
sqDistancePolyPoint ps p
  | insidePolygon ps p = 0
  | otherwise = minimum [sqDistanceSP s p | s <- polygonSegments ps]

rotatePoint :: Rational -> Rational -> Point -> Point -> Point
rotatePoint sin cos (x,y) = translatePoint x y
                          . rotatePoint0
                          . translatePoint (-x) (-y)
  where
  rotatePoint0 (x,y) = (x*cos - y*sin, x*sin + y*cos)

-- I think this is not working (see tests)
rotateLine0 :: Rational -> Rational -> Line -> Line
rotateLine0 sin cos (a,b,c) = ( cos * a - sin * b
                              , sin * a + cos * b
                              , c
                              )

rotateLine :: Point -> Rational -> Rational -> Line -> Line
rotateLine (x,y) sin cos = translateLine x y
                         . rotateLine0 sin cos
                         . translateLine (-x) (-y)

translatePoint :: Rational -> Rational -> Point -> Point
translatePoint x y (x0,y0) = (x0 + x, y0 + y)

translateLine :: Rational -> Rational -> Line -> Line
translateLine x y (a,b,c) = (a, b, 1 - a*x - b*y)

polygonSegments :: Polygon -> [Segment]
polygonSegments []    = error "polygonSegments: not a polygon"
polygonSegments [_]   = error "polygonSegments: not a polygon"
polygonSegments [_,_] = error "polygonSegments: not a polygon"
polygonSegments ps = zip ps (tail ps ++ [head ps])

-- | computes the bounding box of a segment
bboxS :: Segment -> Box
bboxS ((x0,y0),(x1,y1)) = ( (min x0 x1, min y0 y1)
                          , (max x0 x1, max y0 y1) )

-- | computes the bounding box of a circle
bboxC :: Circle -> Box
bboxC ((x,y),r) = ((x-r,y-r),(x+r,y+r))

grow :: Rational -> Box -> Box
grow r ((x0,y0),(x1,y1)) = ((x0-r,y0-r),(x1+r,y1+r))


-- -- misc utilities -- --
between :: Rational -> (Rational,Rational) -> Bool
x `between` (y,z) = y <= x && x <= z
                 || z <= x && x <= y

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
