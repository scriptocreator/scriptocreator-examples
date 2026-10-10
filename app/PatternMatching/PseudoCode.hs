module PseudoCode  where

import RealCode ((<+>))
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
