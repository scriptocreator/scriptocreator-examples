module SuperCode where

import Sample
import Scriptocreator.MayEither

import Data.Word
import Data.Bits
import Data.Vector.Strict as V
import Data.Vector.Unboxed as VU
import Data.Vector.Generic as G
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Maybe
import Text.Printf


type ListMap = (Bool, Word64, Int)
type Tag = Int


argsBD :: V.Vector (VU.Vector ListMap)
argsBD = V.fromList [arg1, arg2]

nesArgsBD :: V.Vector (V.Vector (VU.Vector ListMap))
nesArgsBD = V.fromList [V.fromList [arg1field1]]

arg1 = VU.fromList
    [(True, bit 0, 0)]

arg1field1 = VU.fromList
    [(False, bit 2, 0)
    ,(False, bit 1, 0)]

arg2 = VU.fromList
    [(False, bit 0, 0)
    ,(False, bit 1 .|. bit 2, 0)]


data Cons
    = ConsVoid Tag
    | Cons Tag (V.Vector Cons)
    deriving (Show, Eq, Ord)

(<+>) = (.&.)
(<->) a b = a .&. complement b
(<++>) = (.|.)

isEmpty = (== 0)
firstDef = countTrailingZeros


process :: V.Vector Cons -> V.Vector (VU.Vector ListMap) -> Word64
process values args = case result of
    MyLeft err -> error err
    MyRight res -> res

    where pars :: V.Vector (Cons, VU.Vector ListMap)
          pars = G.zip values args

          result = G.foldl' calc MyNothing pars


err = "Нарушение логики: (%s) != (%s)"

calc :: MayEither String Word64 -> (Cons, VU.Vector ListMap) -> MayEither String Word64

calc MyNothing cons@(ConsVoid tag, argMass)
    = case argMass VU.! pred tag of
        (False, defs, _) -> MyRight defs

        insErr -> MyLeft $ printf err (show cons) (show insErr)

calc (MyRight predDefs) cons@(ConsVoid tag, argMass)
    = case argMass VU.! pred tag of
        (False, defs, _) -> MyRight $ predDefs <+> defs

        insErr -> MyLeft $ printf err (show cons) (show insErr)

calc MyNothing cons@(Cons tag nesTags, argMass) = case argMass VU.! pred tag of
    (True, defs, nesArgsIndMass) ->
        let nesArgMass = nesArgsBD V.! nesArgsIndMass
            nesteds = G.zip nesTags nesArgMass
        in fmap (defs<++>) $ G.foldl' calc MyNothing nesteds

    insErr -> MyLeft $ printf err (show cons) (show insErr)

calc (MyRight predDefs) cons@(Cons tag nesTags, argMass) = case argMass VU.! pred tag of
    (True, defs, nesArgsIndMass) ->
        let nesArgMass = nesArgsBD V.! nesArgsIndMass
            nesteds = G.zip nesTags nesArgMass
        in fmap (predDefs<->) $ fmap (defs<++>) $ G.foldl' calc MyNothing nesteds

    insErr -> MyLeft $ printf err (show cons) (show insErr)

calc (MyLeft lineErr) _ = MyLeft lineErr


-- Что равносильно (Left Nothing) + True
values = V.fromList [Cons 1 (V.fromList [ConsVoid 1]), ConsVoid 2]

funcSim :: V.Vector Cons -> String
funcSim ins = (funcBD!!) . firstDef $ process ins argsBD
