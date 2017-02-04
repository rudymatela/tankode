module Tankode.Constants
  ( ticksPerSecond
  , maxSpeed
  , maxAccel
  , maxTurn
  , minTurn
  , maxGunSpeed
  , maxRadarSpeed
  , charging
  , cooling
  , heating
  , bulletMinSpeed
  , bulletMaxSpeed
  , damageFactor
  , tankDiameter
  , tankRadius
  , scanRadius
  , squaredTankDiameter
  , squaredTankRadius
  )
where

import Data.Ratio

ticksPerSecond :: Num a => a
ticksPerSecond = 360

maxAccel, maxSpeed, minTurn, maxTurn, maxGunSpeed, maxRadarSpeed :: Rational
maxAccel      = 1 / ticksPerSecond -- 1 second to reach full speed
maxSpeed      = 2 / ticksPerSecond -- in units per tick
minTurn       = 1/4 / ticksPerSecond
maxTurn       = 2/3 / ticksPerSecond
maxGunSpeed   = 1 / ticksPerSecond -- 1 full turn per second
maxRadarSpeed = maxGunSpeed * 3

charging, cooling, heating :: Rational
charging = 1 / ticksPerSecond -- 1 second to reach full charge
cooling  = 6 / ticksPerSecond -- max 6 shots per second
heating  = 1

damageFactor :: Rational
damageFactor = 1 % 12

bulletMinSpeed, bulletMaxSpeed :: Rational
bulletMinSpeed = maxSpeed * 3
bulletMaxSpeed = maxSpeed * 5

tankDiameter, tankRadius, scanRadius :: Rational
tankDiameter = 1
tankRadius   = tankDiameter / 2
scanRadius = tankDiameter * 30

squaredTankDiameter, squaredTankRadius :: Rational
squaredTankDiameter = tankDiameter * tankDiameter
squaredTankRadius   = tankRadius   * tankRadius
