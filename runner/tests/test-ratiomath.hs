-- Test library
import Test

-- Functions under test
import RatioMath
import Geometry
import Prelude hiding (sin, cos, tan, asin, acos, atan, pi, sqrt)
import Data.Tuple (swap)
import Control.Arrow ((***))
import Data.Function (on)
import Data.List (sort)
import List

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True
  , holds n $ \q -> 0 <= q && q <= 1 ==> sin (asin q) =~= q
  , holds n $ \q -> 0 <= q && q <= 1 ==> cos (acos q) =~= q
  , holds n $ \q -> 0 <= q && q <= 2 ==> tan (atan q) =~~= q
  , holds n $ \th -> -1%4 <= th && th <= 1%4 ==> asin (sin th) =~= th
  , holds n $ \th ->    0 <= th && th <= 1%2 ==> acos (cos th) =~= th
  , holds (n`div`10) $ \q -> 0 <= q ==> sqrt q * sqrt q =~= q

  ,       intersectSS ((0,0),(1,1)) ((1,0),(0,1))
  , not $ intersectSS ((0,0),(1,1)) ((2,2),(3,3))
  ,       intersectSS ((0,0),(1,1)) ((1,1),(2,2))
  , not $ intersectSS ((0,0),(1,1)) ((2,1),(1,2))
  ,       intersectSS ((0,0),(0,0)) ((0,0),(0,0))
  , holds n $ \x y     -> intersectSS ((x,x),(y,y)) ((x,y),(y,x))
  , holds n $ \x y z w -> intersectSS ((x,y),(z,w)) ((x,w),(z,y))
  , holds n $ \s -> intersectSS s s
  , holds n $ \s -> intersectSS s (swap s)
  , holds n $ \p1 p2 p3 -> intersectSS (p1,p2) (p2,p3)
  , holds n $ \s1 s2 -> intersectSS s1 s2 == intersectSS s2 s1
  , holds n $ \s1 s2 -> intersectSS (swap s1) (swap s2) == intersectSS s1 s2
  , holds n $ \s1 s2 -> intersectSS (sswwaapp s1) (sswwaapp s2) == intersectSS s1 s2
  , holds n $ \p1 p2 -> linePoint (lineFromPoints p1 p2) p1 == 0
  , holds n $ \p1 p2 p3 -> collinearity p1 p2 p3 == linePoint (lineFromPoints p1 p2) p3
  , sqDistanceLP (lineFromPoints (0,0) (1,1)) (1,0) == 1%2
  , sqDistanceLP (lineFromPoints (0,0) (1,1)) (0,1) == 1%2
  , holds n $ \x y -> sqDistanceLP (lineFromPoints (0,0) (0,1)) (x,y) == x*x
  , holds n $ \x y -> sqDistanceLP (lineFromPoints (0,0) (1,0)) (x,y) == y*y
-- TODO: make the following pass
--, holds n $ \p l -> l /= (0,0,0) ==> rotateLine p 0 1 l == l
--, rotateLine (0,0) 1 0 (lineFromPoints (0,0) (0,1)) `linePoint` (1,0) == 0
--, rotateLine (0,0) 1 1 (lineFromPoints (0,0) (0,1)) `linePoint` (1,1) == 0
  , sqDistanceSP ((0,0),(1,1)) (1,0) == 1%2
  , sqDistanceSP ((0,0),(1,1)) (0,1) == 1%2
  , sqDistanceSP ((0,0),(1,1)) (1%2,1%2) == 0
  , sqDistanceSP ((0,0),(1,1)) (3%2,3%2) == 1%2
  , sqDistanceSP ((0,0),(1,1)) (-1%2,-1%2) == 1%2
  , sqDistanceSP ((0,0),(1,1)) (2,2) == 2
  , holds n $ \s p -> sqDistanceSP (sswwaapp s) (swap p) == sqDistanceSP s p
  , sqDistanceSP ((1,1),(3,2)) (3,3) == 1
  , sqDistanceSP ((1,1),(3,2)) (2,2) == 1%5
  , sqDistanceSP ((1,1),(3,2)) (2,1) == 1%5
  , sqDistanceSP ((1,1),(3,2)) (2,3%2) == 0

  , holds n $ \x y -> identity $ translatePoint x y . translatePoint (-x) (-y)
  , holds n $ identity $ translatePoint 0 0
  , holds n $ \p -> identity $ rotatePoint 0 1 p
  , holds n $ \p -> identity $ rotatePoint 1 0 p
                             . rotatePoint 1 0 p
                             . rotatePoint 1 0 p
                             . rotatePoint 1 0 p
  , holds n $ \p -> rotatePoint 0 (-1) p === (rotatePoint 1 0 p . rotatePoint 1 0 p)

  , holds n $ \a b c -> insideTriangle (a,b,c) a
  , holds n $ \a b c -> insideTriangle (a,b,c) b
  , holds n $ \a b c -> insideTriangle (a,b,c) c
  , holds n $ \a b c -> insideTriangle (a,b,c) (midpoint a b)
  , holds n $ \a b c -> insideTriangle (a,b,c) (midpoint b c)
  , holds n $ \a b c -> insideTriangle (a,b,c) (midpoint c a)
  , holds n $ \a b c -> insideTriangle (a,b,c) (midpoint a (midpoint b c))
  , holds n $ \a b c -> insideTriangle (a,b,c) (midpoint b (midpoint a c))
  , holds n $ \a b c -> insideTriangle (a,b,c) (midpoint c (midpoint a b))
  ,       insideTriangle ((0,0),(3,0),(0,3)) (1,1)
  , not $ insideTriangle ((0,0),(3,0),(0,3)) (2,2)
  , not $ insideTriangle ((0,0),(3,0),(0,3)) (3,3)
  , not $ insideTriangle ((0,0),(3,0),(0,3)) (10,10)
  , sqDistanceTP ((0,0),(3,0),(0,3)) (2,2) == 1%2
  , sqDistanceTP ((0,0),(3,0),(0,3)) (3,3) == 9%2

-- TODO: fix coincide then fix those tests
--, holds n $ \l -> validL l ==> coincideL l l
  , holds n $ \l1 l2 -> validL l1 && validL l2 ==> coincideL l1 l2 == coincideL l2 l1
--, holds n $ \l@(a,b,c) -> validL l ==> coincideL l (a*2,b*2,c*2)

  , holds n $ \p1 p2 p3 ->
      let l12 = lineFromPoints p1 p2
          l23 = lineFromPoints p2 p3
      in not (parallel l12 l23) ==> intersectionPointLL l12 l23 == Just p2
  , holds n $ \l -> intersectionPointLL l l == Nothing

  -- there are no secant points for the square with an apothem of 1
  , secantPoints0 ((-1, 1),( 1, 1)) 1 == Nothing
  , secantPoints0 (( 1, 1),( 1,-1)) 1 == Nothing
  , secantPoints0 (( 1,-1),(-1,-1)) 1 == Nothing
  , secantPoints0 ((-1,-1),(-1, 1)) 1 == Nothing

  -- secant points of the horizontal line passing through origin
  , secantPoints0 ((-1,0),(1,0)) 1 == Just ((1,0),(-1,0))
  , secantPoints0 ((-2,0),(2,0)) 1 == Just ((1,0),(-1,0))
  , secantPoints0 ( (0,0),(1,0)) 1 == Just ((1,0),(-1,0))
  , secantPoints0 ((-1,0),(1,0)) 2 == Just ((2,0),(-2,0))

  -- secant points of the vertical line passing through origin
  , secantPoints0 ((0,-1),(0,1)) 1 == Just ((0,1),(0,-1))
  , secantPoints0 ((0,-2),(0,2)) 1 == Just ((0,1),(0,-1))
  , secantPoints0 ((0, 0),(0,1)) 1 == Just ((0,1),(0,-1))

  -- diagonals passing through origin
  , secantPoints0 ((-1,-1),( 1, 1)) 1 =/= Just ((cos (1/8), sin (1/8)),(-cos (1/8),-sin (1/8)))
  , secantPoints0 ((-1,-1),( 0, 0)) 1 =/= Just ((cos (1/8), sin (1/8)),(-cos (1/8),-sin (1/8)))
  , secantPoints0 (( 0, 0),( 1, 1)) 1 =/= Just ((cos (1/8), sin (1/8)),(-cos (1/8),-sin (1/8)))
  , secantPoints0 ((-2,-2),( 2, 2)) 1 =/= Just ((cos (1/8), sin (1/8)),(-cos (1/8),-sin (1/8)))
  , secantPoints0 ((-1, 1),( 1,-1)) 1 =/= Just ((cos (1/8),-sin (1/8)),(-cos (1/8), sin (1/8)))

  -- horizontal, 1/2 distance above x axis
  , secantPoints0 ((-1,1/2),(1,1/2)) 1 =/= Just ((-cos (1/12), sin (1/12)), (cos (1/12), sin (1/12)))

  , holds n $ \p1 p2 r -> secantPoints0 (p1,p2) r =.= secantPoints0 (p2,p1) r
  , holds n $ \p1 p2 c -> secantPoints  (p1,p2) c =.= secantPoints  (p2,p1) c
  , holds n $ \p1 p2 c -> secantPointsS (p1,p2) c =:= secantPointsS (p2,p1) c

  -- axis swap
  , holds n $ \s r -> secantPoints0 s r =.= fmap sswwaapp (secantPoints0 (sswwaapp s) r)
  , holds n $ \s c@(p,r) -> secantPoints  s c =.= fmap sswwaapp (secantPoints  (sswwaapp s) (swap p,r))
  , holds n $ \s c@(p,r) -> secantPointsS s c =:= map swap      (secantPointsS (sswwaapp s) (swap p,r))

  , holds n $ \x1 x2 y c -> all ((y ==) . snd) . maybePairToList $ secantPoints ((x1,y),(x2,y)) c
  , holds n $ \x y1 y2 c -> all ((x ==) . fst) . maybePairToList $ secantPoints ((x,y1),(x,y2)) c
  , holds n $ \x1 x2 y c -> all ((y ==) . snd) $ secantPointsS ((x1,y),(x2,y)) c
  , holds n $ \x y1 y2 c -> all ((x ==) . fst) $ secantPointsS ((x,y1),(x,y2)) c

  , secantPoints  ((-1,1/2),(1,1/2)) ((2,1),1) =/= Just ((2 -     cos (30/360), 1/2), (2 +     cos (30/360), 1/2))
  , secantPoints  ((-1,0),(1,0)) ((2,1),2) =/= Just ((2 - 2 * cos (30/360), 0), (2 + 2 * cos (30/360), 0))
  , secantPointsS ((-1,1/2),(1,1/2)) ((2,1),1) =$ map (roundRR 60) $= []
  -- TODO: failing, fix: returning []
  , secantPointsS ((-1,0),(1,0)) ((2,1),2) =$ map (roundRR 60) $= [(2 - 2 * cos (30/360), 0)]
  , secantPointsS ((0,0),(1,0)) ((0,0),1)      =$ map (roundRR 60) $= [(1,0)]

  , mapThat odd  (*10) [0,1,2,3,4,5] == [0,10,2,30,4,50]
  , mapThat even (*10) [0,1,2,3,4,5] == [0,1,20,3,40,5]
  ]
  where
  (=.=) = (==) `on` sortMPair
  (=:=) = (==) `on` sort
  (=/=) = (==) `on` (sortMPair . fmap (roundRRRR 60))
  roundRR   p = roundR  p *** roundR  p
  roundRRRR p = roundRR p *** roundRR p

maybePairToList :: Maybe (a,a) -> [a]
maybePairToList Nothing      = []
maybePairToList (Just (x,y)) = [x,y]

sortMPair :: Ord a => Maybe (a,a) -> Maybe (a,a)
sortMPair = fmap sortPair

sortPair :: Ord a => (a,a) -> (a,a)
sortPair (p1,p2) | p1 > p2   = (p2,p1)
                 | otherwise = (p1,p2)

sswwaapp :: ((a,b),(c,d)) -> ((b,a),(d,c))
sswwaapp = swap *** swap

(=~=) :: Rational -> Rational -> Bool
q =~= r = abs (q - r) <= 6 % precision
infix 4 =~=

(=~~=) :: Rational -> Rational -> Bool
q =~~= r = abs (q - r) <= 1 % 60
infix 4 =~~=
