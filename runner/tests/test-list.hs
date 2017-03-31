-- Test library
import Test

-- Functions under test
import List

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , split ',' "asdf,qwer,,zxcv" == ["asdf", "qwer", "", "zxcv"]
  , split ',' " , " == [" ", " "]
  , split undefined "" == [""]
  , split ';' " , " == [" , "]
  , holds n $ \x xs    -> not (x `elem` xs)
                      ==> split x xs == [xs :: [Int]]
  , holds n $ \x xs ys -> not (x `elem` xs) && not (x `elem` ys)
                      ==> split x (xs ++ [x] ++ ys) == [xs,ys :: [Int]]
  , holds n $ \x xs  ->  join x (split x xs) == (xs :: [Int])
  , holds n $ \x xss -> not (null xss) && all (x `notElem`) xss
                    ==> split x (join x xss) == (xss :: [[Int]])

  , trim ' ' "     " == ""
  , trim ' ' " blah " == "blah"
  , trim ' ' " Hello World " == "Hello World"
  ]
