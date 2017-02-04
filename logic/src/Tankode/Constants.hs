module Tankode.Constants
  ( ticksPerSecond
  , maxSpeed
  , maxAccel
  , maxTurnSpeed
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

maxAccel, maxSpeed, maxTurnSpeed, maxGunSpeed, maxRadarSpeed :: Rational
maxAccel      = 1 / ticksPerSecond -- 1 second to reach full speed
maxSpeed      = 2 / ticksPerSecond -- in units per tick
maxTurnSpeed  = 1 / 2  /  ticksPerSecond
maxGunSpeed   = maxTurnSpeed * 2
maxRadarSpeed = maxGunSpeed * 3

charging, cooling, heating :: Rational
charging = 1 / ticksPerSecond -- 1 second to reach full charge
cooling  = 6 / ticksPerSecond -- max 6 shots per second
heating  = 1

damageFactor :: Rational
damageFactor = 1 % 12

bulletMinSpeed, bulletMaxSpeed :: Rational
bulletMinSpeed = maxSpeed * 3 / 2
bulletMaxSpeed = maxSpeed * 3

tankDiameter, tankRadius, scanRadius :: Rational
tankDiameter = 1
tankRadius   = tankDiameter / 2
scanRadius = tankDiameter * 30

squaredTankDiameter, squaredTankRadius :: Rational
squaredTankDiameter = tankDiameter * tankDiameter
squaredTankRadius   = tankRadius   * tankRadius
