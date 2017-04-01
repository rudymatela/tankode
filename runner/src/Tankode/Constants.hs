-- |
-- Module      : Tankode.Constants
-- Copyright   : (c) 2016, 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Constants used in the Tankode game.
module Tankode.Constants
  ( ticksPerSecond
  , maxSpeed
  , maxAccel
  , maxTurn
  , minTurn
  , maxGunTurn
  , maxRadarTurn
  , charging
  , cooling
  , heating
  , bulletMinSpeed
  , bulletMaxSpeed
  , maxShoot
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
ticksPerSecond = 120

maxAccel, maxSpeed, minTurn, maxTurn, maxGunTurn, maxRadarTurn :: Rational
maxAccel     = 1   / ticksPerSecond -- 1 second to reach full speed
maxSpeed     = 2   / ticksPerSecond -- in units per tick
minTurn      = 1/4 / ticksPerSecond -- body turn speed when at full speed
maxTurn      = 1/2 / ticksPerSecond -- body turn speed when stationary
maxGunTurn   = 2/3 / ticksPerSecond -- 2/3 of a full turn per second
maxRadarTurn = 3   / ticksPerSecond -- 3 full turns per second

charging, cooling, heating :: Rational
charging = 1 / ticksPerSecond -- 1 second to reach full charge
cooling  = 6 / ticksPerSecond -- max 6 shots per second
heating  = 1

damageFactor :: Rational
damageFactor = 1 % 12

bulletMinSpeed, bulletMaxSpeed :: Rational
bulletMinSpeed = maxSpeed * 3
bulletMaxSpeed = maxSpeed * 5

maxShoot :: Rational
maxShoot = 1

tankDiameter, tankRadius, scanRadius :: Rational
tankDiameter = 1
tankRadius   = tankDiameter / 2
scanRadius   = tankDiameter * 30

squaredTankDiameter, squaredTankRadius :: Rational
squaredTankDiameter = tankDiameter * tankDiameter
squaredTankRadius   = tankRadius   * tankRadius
