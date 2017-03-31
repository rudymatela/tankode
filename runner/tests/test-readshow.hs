-- Test library
import Test

-- Functions under test
import Tankode.Show

import Data.Ratio
import Numeric

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \r -> readMR (showMR r) == r
  
  , holds n $ \r -> let r' = readR (showMRFrac r)
                    in denominator r' < 1000 ==> r' == r

  , holds n $ \x -> readR (show x) == (x % 1)
  ]


showMRFrac :: Rational -> String
showMRFrac r = showFFloat Nothing (fromIntegral (numerator r) / fromIntegral (denominator r)) ""
