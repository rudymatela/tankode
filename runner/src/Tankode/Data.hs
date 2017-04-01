-- |
-- Module      : Tankode.Data
-- Copyright   : (c) 2016, 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Datatype definitions used in the Tankode game.
module Tankode.Data
--( Tank (..)
--, TankId (..)
--, TankStatus (..)
--, TankPos (..)
--, Field (..)
--, module Colour
--)
where

import Colour (Colour)
import Tankode.Palette as P
import Data.Ratio
import Control.Arrow ((***))
import Prelude hiding (sin, cos, asin, acos, sqrt)
import RatioMath
import Geometry
import System.Posix.Types (ProcessID)

type Loc = Point

-- data Invariant: convex polygons only
-- >= 3 points
type Obstacle = [Loc]

data Field = Field
  { width  :: Rational
  , height :: Rational
  , obstacles :: [Obstacle]
  , fieldColour :: Colour
  , obstacleColour :: Colour
  }
  deriving Show

makeField :: Rational -> Rational -> Field
makeField w h = Field
  { width = w
  , height = h
  , obstacles = -- obstacles on border, to avoid special case
    [ [(- w, h), (w + w, h), (w / 2, h + h)] -- top
    , [(- w, 0), (w + w, 0), (w / 2,   - h)] -- bottom
    , [(0, - h), (0, h + h), (  - w, h / 2)] -- left
    , [(w, - h), (w, h + h), (w + w, h / 2)] -- right
    ]
  , fieldColour = P.black
  , obstacleColour = P.darkGrey
  }

data Tank = Tank
  { name :: String

  , tankode :: Tankode -- ^ AI
  , pid :: ProcessID

  , trackColour  :: Colour
  , bodyColour   :: Colour
  , gunColour    :: Colour
  , radarColour  :: Colour
  , bulletColour :: Colour
  , scanColour   :: Colour

  , loc     :: Loc
  , heading :: Rational
  , gun     :: Rational -- ^ gun   heading relative to body, use 'gunHeading'   for absolute heading
  , radar   :: Rational -- ^ radar heading relative to gun,  use 'radarHeading' for absolute heading
  , speed   :: Rational

  , integrity :: Rational
  , power     :: Rational
  , heat      :: Rational
  , bullets   :: [Bullet]
  }
--deriving Show -- not possible because of tankode field

-- Tank initialized with default values
tank :: Tank
tank = Tank
  { name = "noname"

  , tankode = error "no tankode"
  , pid = error "no pid"

  , trackColour  = grey4
  , bodyColour   = grey3
  , gunColour    = grey6
  , radarColour  = grey3
  , bulletColour = grey9
  , scanColour   = grey3

  , loc     = (0,0) -- TODO: change to error later?
  , heading = 0     -- TODO: change to error later?
  , gun     = 0
  , radar   = 0
  , speed   = 0

  , integrity = 1
  , power     = 0
  , heat      = 0
  , bullets   = []
  }

data Bullet = Bullet
  { bulletCharge   :: Rational
  , bulletLoc      :: Loc
  , bulletHeading  :: Rational
  , bulletExploded :: Bool
  }
  deriving Show

bullet :: Rational -> Loc -> Rational -> Bullet
bullet c l h = Bullet c l h False

--                    life      speed     enemy           wall
type TankodeInput  = (Rational, Rational, Maybe Rational, Maybe Rational)
--                    accel     body      gun       radar     shoot
type TankodeOutput = (Rational, Rational, Rational, Rational, Rational)
-- Provisional type (this will become IO eventually):
type Tankode       = TankodeInput -> IO TankodeOutput

gunHeading, radarHeading :: Tank -> Rational
gunHeading   t =   gun t +    heading t
radarHeading t = radar t + gunHeading t

updateHeading, updateGun, updateRadar :: (Rational -> Rational) -> Tank -> Tank
updateHeading f t = t {heading = f $ heading t}
updateGun     f t = t {gun     = f $ gun     t}
updateRadar   f t = t {radar   = f $ radar   t}

updateLoc :: (Loc -> Loc) -> Tank -> Tank
updateLoc f t = t {loc = f $ loc t}

updateSpeed :: (Rational -> Rational) -> Tank -> Tank
updateSpeed f t = t {speed = f $ speed t}

updateBulletLoc :: (Loc -> Loc) -> Bullet -> Bullet
updateBulletLoc f t = t {bulletLoc = f $ bulletLoc t}

updateBullets :: ([Bullet] -> [Bullet]) -> Tank -> Tank
updateBullets f t = t {bullets = f $ bullets t}

updatePower :: (Rational -> Rational) -> Tank -> Tank
updatePower f t = t {power = f $ power t}

updateHeat :: (Rational -> Rational) -> Tank -> Tank
updateHeat f t = t {heat = f $ heat t}

updateIntegrity :: (Rational -> Rational) -> Tank -> Tank
updateIntegrity f t = t {integrity = f $ integrity t}

updateObstacles :: ([Obstacle] -> [Obstacle]) -> Field -> Field
updateObstacles g f = f {obstacles = g $ obstacles f}

addObstacles :: [Obstacle] -> Field -> Field
addObstacles os = updateObstacles (++ os)

turn, turnGun, turnRadar :: Rational -> Tank -> Tank
turn      th = updateHeading (+ th)
turnGun   th = updateGun     (+ th)
turnRadar th = updateRadar   (+ th)

translate :: Rational -> Rational -> Loc -> Loc
translate th' d (x,y) = (x + d * cos th, y + d * sin th) where th = th' + 1%4

translateTank :: Rational -> Rational -> Tank -> Tank
translateTank th d = updateLoc $ translate th d

translateBullet :: Rational -> Rational -> Bullet -> Bullet
translateBullet th d = updateBulletLoc $ translate th d

-- unused: remove?
translate' :: Vector -> Point -> Point
translate' (xd, yd) (x,y) = (xd + x, yd + y)

-- unused: remove?
translateTank' :: Vector -> Tank -> Tank
translateTank' xy = updateLoc $ translate' xy

segments :: [Obstacle] -> [Segment]
segments = concatMap polygonSegments

explode :: Bullet -> Bullet
explode b = b{bulletExploded = True}
