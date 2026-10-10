module RealCode where

import Sample

import Data.IntMap.Strict as Map
import Data.IntSet as Set
import Data.Maybe


data ListMap
  = List IntSet
  | ListCons IntSet [Map.IntMap ListMap]
  deriving (Show, Eq, Ord)

type Tag = Int


arg1 = Map.fromList
    [(1, ListCons (Set.fromList [0]) $ pure arg1field1)]

arg1field1 = Map.fromList
    [(2, List (Set.fromList [1]))
    ,(1, List (Set.fromList [2]))]

arg2 = Map.fromList
    [(1, List (Set.fromList [0]))
    ,(2, List (Set.fromList [1, 2]))]


data Cons
    = ConsVoid Tag
    | Cons Tag [Cons]
    deriving (Show, Eq, Ord)

(<+>) = Set.intersection
(<->) = Set.difference
(<++>) = Set.union


-- Что равносильно (Left Nothing) + True
values = [Cons 1 [ConsVoid 1], ConsVoid 2]

process :: [Cons] -> [Map.IntMap ListMap] -> IntSet
process values args = fromJust $ Prelude.foldl' calc Nothing pars

    where pars :: [(Cons, Map.IntMap ListMap)]
          pars = zip values args

          calc :: Maybe IntSet -> (Cons, Map.IntMap ListMap) -> Maybe IntSet
          calc mPredDefs (value, argMass)
            = case value of
                ConsVoid tag -> case el tag of
                    Just (List defs) -> Just $ updDefs defs $ predDefs <+> defs
                    _ -> error "Нарушение логики"

                Cons tag nesTags -> case el tag of
                    Just (ListCons defs nesArgsMass) ->
                        let nesteds = zip nesTags nesArgsMass
                        in fmapDefs $ fmap (defs<++>) $ Prelude.foldl' calc Nothing nesteds

                    _ -> error "Нарушение логики"
            
            where el tag = Map.lookup tag argMass
                  predDefs = fromJust mPredDefs
                  fmapDefs :: Maybe IntSet -> Maybe IntSet
                  fmapDefs mDefs = if isJust mPredDefs then fmap (predDefs<->) mDefs else mDefs
                  updDefs mDefs newDefs = if isJust mPredDefs then newDefs else mDefs

funcSim :: [Cons] -> String
funcSim ins = (funcBD!!) . Set.findMin $ process ins [arg1, arg2]
