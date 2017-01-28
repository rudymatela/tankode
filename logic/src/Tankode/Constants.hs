module Tankode.Constants
  ( maxSpeed
  , maxAccel
  , maxTurnSpeed
  , maxGunSpeed
  , maxRadarSpeed
  , charging
  , cooling
  , heating
  , bulletSpeed
  , tankDiameter
  , tankRadius
  , scanRadius
  , squaredTankDiameter
  , squaredTankRadius
  )
where

import Data.Ratio

maxAccel, maxSpeed, maxTurnSpeed, maxGunSpeed, maxRadarSpeed :: Rational
maxAccel      = 1 % 360 -- 1 second to reach full speed
maxSpeed      = 2 % 360 -- in units per tick
maxTurnSpeed  = 1 / 2  /  360
maxGunSpeed   = maxTurnSpeed * 2
maxRadarSpeed = maxGunSpeed * 3

charging, cooling, heating :: Rational
charging = 1 % 360
cooling  = 3 % 360
heating  = 1

-- TODO: bulletSpeed in function of charge/power -- more power = less speed
bulletSpeed :: Rational
bulletSpeed = 6 % 360

tankDiameter, tankRadius, scanRadius :: Rational
tankDiameter = 1
tankRadius   = tankDiameter / 2
scanRadius = tankDiameter * 30

squaredTankDiameter, squaredTankRadius :: Rational
squaredTankDiameter = tankDiameter * tankDiameter
squaredTankRadius   = tankRadius   * tankRadius
