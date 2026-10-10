module PseudoCode  where

import Sample


data Tag = Tag Int [Tag] deriving Show


arg1 (Tag 1 (nes:_)) = 0 : arg1field1 nes

arg1field1 (Tag 2 _) = pure 1
arg1field1 (Tag 1 _) = pure 2

arg2 (Tag 1 _) = pure 0
arg2 (Tag 2 _) = [1, 2]


funcSim :: Tag -> Tag -> String
funcSim a b = (funcBD!!) . head $ arg1 a <+> arg2 b

--order f a b
--  | a >= b = f a b
--  | otherwise = f b a

infixl 6 <+>
infixl 6 <->

(<+>) :: Eq a => [a] -> [a] -> [a]
(<+>) [] _ = []
(<+>) _ [] = []
(<+>) (x:xs) l
    | elem x l = x : future
    | otherwise = future
    where future = xs <+> l

(<->) :: Eq a => [a] -> [a] -> [a]
(<->) [] l = l
(<->) xxs [] = xxs
(<->) (x:xs) l
    | elem x l = future
    | otherwise = x : future
    where future = xs <-> l
