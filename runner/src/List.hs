module List
  ( module Data.List
  , discard
  , discardLater
  , choices
  , choicesWith
  , mapChoices
  , maybeMinimum
  , compose
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
