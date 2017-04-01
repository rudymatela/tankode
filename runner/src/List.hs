-- |
-- Module      : List
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Additional list functions to complement the functionality from "Data.List".
module List
  ( module Data.List
  , discard
  , discardLater
  , choices
  , choicesWith
  , mapChoices
  , maybeMinimum
  , compose
  , mapThat
  , gsub
  , pairwise
  , split
  , join
  , trim
  )
where

import Data.List

discard :: (a -> Bool) -> [a] -> [a]
discard f = filter (not . f)

discardLater :: (a -> a -> Bool) -> [a] -> [a]
discardLater d []     = []
discardLater d (x:xs) = x : discardLater d (discard (`d` x) xs)

choices :: [a] -> [(a,[a])]
choices = choicesWith (,)

choicesWith :: (a -> [a] -> b) -> [a] -> [b]
choicesWith f = m []
  where
  m ys []     = []
  m ys (x:xs) = f x (ys ++ xs) : m (ys++[x]) xs

mapChoices :: (a -> [a] -> [a]) -> [a] -> [a]
mapChoices f = m []
  where
  m ys []     = ys
  m ys (x:xs) = let ys' = f x ys
                    xs' = f x xs
                in  m (ys'++[x]) xs'

maybeMinimum :: Ord a => [a] -> Maybe a
maybeMinimum [] = Nothing
maybeMinimum xs = Just $ minimum xs

compose :: [a -> a] -> a -> a
compose [] = id
compose (f:fs) = f . compose fs

mapThat :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapThat p f = map (\x -> if p x then f x else x)

gsub :: Eq a => a -> a -> [a] -> [a]
gsub _ _ []     = []
gsub y z (x:xs) = x' : gsub y z xs
  where
  x' | x == y    = z
     | otherwise = x

pairwise :: [a] -> [(a,a)]
pairwise []       = []
pairwise [_]      = error "pairwise: odd number of values"
pairwise (x:y:xs) = (x,y):pairwise xs

split :: Eq a => a -> [a] -> [[a]]
split x xs = case break (== x) xs of
               (ys,[])   -> [ys]
               (ys,_:zs) -> ys:split x zs

join :: a -> [[a]] -> [a]
join x = intercalate [x]

-- | dropWhile == x on both ends
--
-- > trim ' ' " blah " == "blah"
-- > trim ' ' " Hello World " == "Hello World"
trim :: Eq a => a -> [a] -> [a]
trim x = reverse . dropWhile (== x)
       . reverse . dropWhile (== x)
-- TODO: make this prettier and more efficient
