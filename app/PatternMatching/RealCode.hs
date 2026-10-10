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


data Step = Step
    { el :: Int -> Maybe ListMap
    , predDefs :: IntSet
    , fmapDefs :: Maybe IntSet -> Maybe IntSet
    , updDefs :: IntSet -> IntSet -> IntSet}

compute :: Maybe IntSet -> Map.IntMap ListMap -> Step
compute mPredDefs argMass
    = Step el predDefs fmapDefs updDefs
    
    where el tag = Map.lookup tag argMass
          predDefs = fromJust mPredDefs
          fmapDefs mDefs = if isJust mPredDefs then fmap (predDefs<->) mDefs else mDefs
          updDefs defs newDefs = if isJust mPredDefs then newDefs else defs


process :: [Cons] -> [Map.IntMap ListMap] -> IntSet
process values args = fromJust $ Prelude.foldl' calc Nothing pars

    where pars :: [(Cons, Map.IntMap ListMap)]
          pars = zip values args

          calc :: Maybe IntSet -> (Cons, Map.IntMap ListMap) -> Maybe IntSet
          calc mPredDefs (ConsVoid tag, argMass) = case el s tag of
            Just (List defs) -> Just $ updDefs s defs $ predDefs s <+> defs

            _ -> error "Нарушение логики"

            where s = compute mPredDefs argMass

          calc mPredDefs (Cons tag nesTags, argMass) = case el s tag of
            Just (ListCons defs nesArgsMass) ->
                let nesteds = zip nesTags nesArgsMass
                in fmapDefs s $ fmap (defs<++>) $ Prelude.foldl' calc Nothing nesteds

            _ -> error "Нарушение логики"
            
            where s = compute mPredDefs argMass

-- Что равносильно (Left Nothing) + True
values = [Cons 1 [ConsVoid 1], ConsVoid 2]

funcSim :: [Cons] -> String
funcSim ins = (funcBD!!) . Set.findMin $ process ins [arg1, arg2]
