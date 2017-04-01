-- |
-- Module      : Tankode.Physics
-- Copyright   : (c) 2016, 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Tankode game physics.
module Tankode.Physics where

import Data.Ratio
import List hiding (intersect)
import Data.Function
import Control.Arrow ((***))
import RatioMath
import Geometry
import Prelude hiding (sin, cos, asin, acos, sqrt)
import Tankode.Data
import Tankode.Constants
import Random
import Data.Maybe

type State = [Tank]

bulletSpeed :: Bullet -> Rational
bulletSpeed Bullet{bulletCharge = c} =
  (1-c) * (bulletMaxSpeed - bulletMinSpeed) + bulletMinSpeed

turnSpeed :: Tank -> Rational
turnSpeed Tank{speed = s} =
  (1 - abs s) * (maxTurn - minTurn) + minTurn

bulletDamage :: Bullet -> Rational
bulletDamage Bullet{bulletCharge = c} =
  ((c + c*c) / 2) * damageFactor

-- hitting a wall at full speed is equivalent to a full charge bullet hit
-- two tanks crashing at full speed, is equivalent to 2 full charge bullet hits
collisionDamage :: Tank -> Rational
collisionDamage Tank{speed = s} =
  ((abs s + s*s) / 2) * damageFactor

circle :: Tank -> Circle
circle t = (loc t, 1%2)

intersectTS :: Tank -> Segment -> Bool
intersectTS = intersectCS . circle

walk :: Tank -> Tank
walk t = translateTank (heading t) (speed t * maxSpeed) t

sqDistanceTT :: Tank -> Tank -> Rational
sqDistanceTT = sqDistancePP `on` loc

distanceTT :: Tank -> Tank -> Rational
distanceTT = sqrt .: sqDistanceTT where (.:) = (.) . (.)

collide :: Tank -> Tank -> Bool
collide t1 t2 = intersectCC (circle t1) (circle t2)

hit :: Bullet -> Tank -> Bool
hit b t = sqDistancePP (bulletLoc b) (loc t) <= 1/4

scanTank :: Tank -> State -> Maybe Rational
scanTank t = fmap (subtract tankRadius)
           . maybeMinimum
           . map (distancePP p)
           . concatMap (secantPointsS scanSegment)
           . discard (circle t ==)
           . map circle
  where
  p = loc t
  scanSegment = (p, translate (radarHeading t) scanRadius $ p)

scanTank1 :: Tank -> Tank -> Bool
scanTank1 t t' =
  sqDistancePP (translate (radarHeading t) d $ loc t) (loc t') <= 1%4
  where
  d = distanceTT t t'
-- note this is an approximation.  The actual formula to compute this involves
-- sin/cos/tan.  TODO: change to this formula
-- I can actually compute this without tan:
-- just project two lines separated by 1 unit parallel to the scan
-- if the tank fall within them, then it is scanned

scanObstacle :: Tank -> Field -> Maybe Rational
scanObstacle t f = fmap (subtract tankRadius)
                 . maybeMinimum
                 . map (distancePP p)
                 . mapMaybe (intersectionPointSS scanSegment)
                 $ segments (obstacles f)
  where
  p = loc t
  scanSegment = (p, translate (radarHeading t) scanRadius $ p)

scanObstacleOrInactiveTank :: Tank -> Field -> State -> Maybe Rational
scanObstacleOrInactiveTank t f ts =
  case (scanTank t $ filter inactive ts, scanObstacle t f) of
    (Nothing, Nothing) -> Nothing
    (Just td, Nothing) -> Just td
    (Nothing, Just od) -> Just od
    (Just td, Just od) -> Just $ min td od

scan :: Tank -> Field -> State -> (Maybe Rational, Maybe Rational)
scan t f ts =
  case (scanTank t $ discard inactive ts, scanObstacleOrInactiveTank t f ts) of
    (Nothing, Nothing) -> (Nothing, Nothing)
    (Just td, Nothing) -> (Just td, Nothing)
    (Nothing, Just od) -> (Nothing, Just od)
    -- only the closest object appears:
    (Just td, Just od) | td < od   -> (Just td, Nothing)
                       | td > od   -> (Nothing, Just od)
                       | otherwise -> (Just td, Just od)

shoot :: Rational -> Tank -> Tank
shoot p t
  | p == 0 || p > power t || heat t > 0 = t
  | otherwise = updatePower   (subtract p)
              . updateHeat    (+ heating)
              $ updateBullets (fly (bullet p lo th):) t
    where
    th = gunHeading t
    lo = translate th tankRadius $ loc t
    -- need to start flying right away or will detect collision with shooter?

charge :: Tank -> Tank
charge = updatePower $ min 1 . (+ charging)

cool :: Tank -> Tank
cool = updateHeat $ max 0 . subtract cooling

flyBullets :: Tank -> Tank
flyBullets = updateBullets (mapThat (not . bulletExploded) fly)

fly :: Bullet -> Bullet
fly b = translateBullet (bulletHeading b) (bulletSpeed b) b

-- should be done before fly!  or maybe called within fly??
processHits :: Field -> [Tank] -> [Tank]
processHits f ts = compose (map damageTanks allBullets)
                 $ map ph1 ts
  where
  ph1 :: Tank -> Tank
  ph1 = updateBullets
      $ mapThat (\b -> hitObstacle b || hitAnyTank b) explode
      . discard bulletExploded
  hitAnyTank b = any (b `hitTank`) ts
  hitObstacle :: Bullet -> Bool
  hitObstacle b = any (intersectSS ((bulletLoc b),(bulletLoc $ fly b)))
                $ segments (obstacles f)
  allBullets = discard bulletExploded $ concatMap bullets ts

damageTanks :: Bullet -> [Tank] -> [Tank]
damageTanks b = map (damageTank b)

damageTank :: Bullet -> Tank -> Tank
damageTank b t =
  if sqDistancePP (bulletLoc b) (loc t) <= squaredTankRadius
    then updateIntegrity ((`max` 0) . subtract (bulletDamage b)) t
    else t

hitTank :: Bullet -> Tank -> Bool
hitTank b t = sqDistancePP (bulletLoc b) (loc t) <= squaredTankRadius
-- TODO: (hitTank) check trajectory segment of Tick (not just dest. point)

-- process crashes (between obstacles and tanks)
processCrashes :: Field -> [Tank] -> [Tank]
processCrashes f ts = map pc1 ts
  where
  pc1 :: Tank -> Tank
  pc1 t = if crashes $ walk t
            then t {speed = 0, integrity = max 0 (integrity t - collisionDamage t)}
            else t
  crashes :: Tank -> Bool
  crashes t = any (t `intersectTS`) $ segments (obstacles f)

processCollisions :: [Tank] -> [Tank]
processCollisions = choicesWith pc1 . mapChoices pd1
  where
  pd1 :: Tank -> [Tank] -> [Tank]
  pd1 t ts = if any (guilty t) ts
               then map (\t' -> t' {integrity = max 0 (integrity t' - collisionDamage t)}) ts
               else ts
  pc1 :: Tank -> [Tank] -> Tank
  pc1 t ts = if any (guilty t) ts
               then t {speed = 0, integrity = max 0 (integrity t - collisionDamage t)}
               else t
  guilty t0 t1 = walk t0 `collide` walk t1
              && not (notGuilty t0 t1)
  notGuilty t0 t1 = not (walk t0 `collide` t1)
                 && walk t1 `collide` walk t0
                 && walk t1 `collide` t0 -- not sure if needed

accelerate :: Rational -> Tank -> Tank
accelerate 0 t = t
accelerate a t@Tank{speed = s}
  | a > 0 && s >= -maxAccel = t{speed = (s +     maxAccel) `min`   1 }
  | a > 0 && s <  -maxAccel = t{speed = (s + 2 * maxAccel) `min`   1 }
  | a < 0 && s <=  maxAccel = t{speed = (s -     maxAccel) `max` (-1)}
  | a < 0 && s >   maxAccel = t{speed = (s - 2 * maxAccel) `max` (-1)}

-- | asks the tankode for an action and perform it when possible
act :: Tank -> Field -> State -> IO Tank
act t f ts | inactive t = do
  _ <- tankode t (0, 0, Nothing, Nothing)
  return $ t {tankode = \_ -> return $ error "act: Tankode terminated"}
act t f ts = do
  (accel, bt, gt, rt, s) <- tankode t (integrity t, speed t, enemy, wall)
  return . turnRadar rt
         . turnGun   gt
         . turn      (bt * turnSpeed t)
         . shoot s
         . accelerate accel
         . walk
         $ t
  where
  (enemy, wall) = scan t f ts

nextState :: Field -> State -> IO State
nextState f = performActions
            . map (cool . charge . flyBullets)
            . processCollisions
            . processCrashes f
            . processHits f
  where
  performActions :: State -> IO State
  performActions ts = traverse (\t -> act t f ts) ts

-- FIXME: at some point in the generated list, this will not terminate
startingPositions :: RandomGen g => Field -> g -> [(Rational,Rational)]
startingPositions f gen = id
  . discardLater (\b a -> distancePP a b <= squaredTankDiameter)
  . discard touchesObstacles
  . map ((%2)***(%2))
  $ randomRs ((0,0), (floor (width f) * 2,floor (height f) * 2)) gen
  where
  touchesObstacles p = any (\o -> sqDistancePolyPoint o p <= squaredTankRadius)
                     $ obstacles f

startingHeadings :: RandomGen g => g -> [Rational]
startingHeadings = map (%24) . randomRs (0,24)

inactive :: Tank -> Bool
inactive t = integrity t <= 0

nActive :: [Tank] -> Int
nActive = length . filter (not . inactive)
