module RealCode where

import Sample

import Data.Map.Strict as Map
import Data.Maybe


data ListMap
  = List [Int]
  | ListCons [Int] [Map.Map Tag ListMap]
  deriving (Show, Eq, Ord)

data Tag = Tag Int deriving (Show, Eq, Ord)


arg1 = Map.fromList
    [(Tag 1, ListCons [0] $ pure arg1field1)]

arg1field1 = Map.fromList
    [(Tag 2, List $ pure 1)
    ,(Tag 1, List $ pure 2)]

arg2 = Map.fromList
    [(Tag 1, List $ pure 0)
    ,(Tag 2, List [1, 2])]


data Cons
    = ConsVoid Tag
    | Cons Tag [Cons]
    deriving (Show, Eq, Ord)

infixl 6 <+>

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


-- Что равносильно (Left Nothing) + True
values = [Cons (Tag 1) [ConsVoid (Tag 1)], ConsVoid (Tag 2)]

process :: [Cons] -> [Map.Map Tag ListMap] -> [Int]
process values args = fromJust $ Prelude.foldl calc Nothing pars

    where pars :: [(Cons, Map.Map Tag ListMap)]
          pars = zip values args

          calc :: Maybe [Int] -> (Cons, Map.Map Tag ListMap) -> Maybe [Int]
          calc mPredDefs (value, argMass)
            = case value of
                ConsVoid tag -> case el tag of
                    Just (List defs) -> Just $ updDefs defs $ predDefs <+> defs
                    _ -> error "Нарушение логики"

                Cons tag nesTags -> case el tag of
                    Just (ListCons [] nesArgsMass) ->
                        let nesteds = zip nesTags nesArgsMass
                        in Prelude.foldl calc mPredDefs nesteds

                    Just (ListCons defs nesArgsMass) ->
                        let nesteds = zip nesTags nesArgsMass
                            newMDefs = Just $ updDefs defs $ predDefs <-> defs
                        in Prelude.foldl calc newMDefs nesteds

                    _ -> error "Нарушение логики"
            
            where el tag = Map.lookup tag argMass
                  predDefs = fromJust mPredDefs
                  updDefs defs newDefs = if isJust mPredDefs then newDefs else defs

funcSim :: [Cons] -> String
funcSim ins = (funcBD!!) . head $ process ins [arg1, arg2]
