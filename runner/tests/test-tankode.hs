-- Test library
import Test

-- Functions under test
import Tankode
import Tankode.Constants
import List

-- Utils
import Data.Ratio

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , squaredTankRadius   == 1%4
  , squaredTankDiameter == tankDiameter

  , choices ([]::[Int]) == []
  , choices [1]         == [(1,[])]
  , choices [1,2]       == [(1,[2]),(2,[1])]
  , choices [1,2,3]     == [(1,[2,3]),(2,[1,3]),(3,[1,2])]
  ]
