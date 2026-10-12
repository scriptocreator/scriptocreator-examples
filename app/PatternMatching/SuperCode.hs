module SuperCode where

import Sample

import Data.Word
import Data.Bits
import Data.Vector.Unboxed as V
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Maybe


type ListMap = (Bool, Word64, Int)
type Tag = Int


argsBD :: [V.Vector ListMap]
argsBD = [arg1, arg2]

nesArgsBD :: [[V.Vector ListMap]]
nesArgsBD = [[arg1field1]]

arg1 = V.fromList
    [(True, bit 0, 0)]

arg1field1 = V.fromList
    [(False, bit 1, 0)
    ,(False, bit 2, 0)]

arg2 = V.fromList
    [(False, bit 0, 0)
    ,(False, bit 1 .|. bit 2, 0)]


data Cons
    = ConsVoid Tag
    | Cons Tag [Cons]
    deriving (Show, Eq, Ord)

(<+>) = (.&.)
(<->) a b = a .&. complement b
(<++>) = (.|.)

isEmpty = (== 0)
firstDef = countTrailingZeros


data Step = Step
    { el :: Int -> ListMap
    , predDefs :: Word64
    , fmapDefs :: Maybe Word64 -> Maybe Word64
    , updDefs :: Word64 -> Word64 -> Word64}

compute :: Maybe Word64 -> V.Vector ListMap -> Step
compute mPredDefs argMass
    = Step el predDefs fmapDefs updDefs
    
    where el tag = argMass V.! pred tag
          predDefs = fromJust mPredDefs
          fmapDefs mDefs = if isJust mPredDefs then fmap (predDefs<->) mDefs else mDefs
          updDefs defs newDefs = if isJust mPredDefs then newDefs else defs


process :: [Cons] -> [V.Vector ListMap] -> Word64
process values args = fromJust $ Prelude.foldl' calc Nothing pars

    where pars :: [(Cons, V.Vector ListMap)]
          pars = Prelude.zip values args


calc :: Maybe Word64 -> (Cons, V.Vector ListMap) -> Maybe Word64

calc mPredDefs (ConsVoid tag, argMass) = case el s tag of
    (False, defs, _) -> Just $ updDefs s defs $ predDefs s <+> defs

    _ -> error "Нарушение логики"

    where s = compute mPredDefs argMass

calc mPredDefs (Cons tag nesTags, argMass) = case el s tag of
    (True, defs, nesArgsMass) ->
        let nesteds = Prelude.zip nesTags $ nesArgsBD !! nesArgsMass
        in fmapDefs s $ fmap (defs<++>) $ Prelude.foldl' calc Nothing nesteds

    _ -> error "Нарушение логики"

    where s = compute mPredDefs argMass


-- Что равносильно (Left Nothing) + True
values = [Cons 1 [ConsVoid 1], ConsVoid 2]

funcSim :: [Cons] -> String
funcSim ins = (funcBD!!) . firstDef $ process ins argsBD
