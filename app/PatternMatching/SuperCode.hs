module SuperCode where

import Sample
import Scriptocreator.MayEither

import Data.Word
import Data.Bits
import Data.Vector.Strict as V
import Data.Vector.Unboxed as VU
import Data.Vector.Generic as G
import Data.IORef
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Maybe as M
import Text.Printf
import Data.Map as Map


vec = V.fromList
val = Value
prom = Promise
thk el = newIORef $ Thunk $ \() -> el

type ListMap = (Bool, Word64, Int)
type Tag = Int


argsBD :: V.Vector (VU.Vector ListMap)
argsBD = vec [arg1, arg2]

nesArgsBD :: V.Vector (V.Vector (VU.Vector ListMap))
nesArgsBD = vec [vec [arg1field1]]

arg1 = VU.fromList
    [(True, bit 0, 0)]

arg1field1 = VU.fromList
    [(False, bit 2, 0)
    ,(False, bit 1, 0)]

arg2 = VU.fromList
    [(False, bit 0, 0)
    ,(False, bit 1 .|. bit 2, 0)]


data Thunk a = Promise (IORef (Promise a)) | Value !a
data Promise a = Thunk (() -> a) | Strict !a

type Cons = Thunk BaldCons
data BaldCons
    = ConsVoid !Tag
    | Cons !Tag !(V.Vector Cons)

(<+>) = (.&.)
(<->) a b = a .&. complement b
(<++>) = (.|.)

isEmpty = (== 0)
firstDef = countTrailingZeros


lambdaIO a b = thunk a b >>= uncurry calc
err = "Нарушение логики: Cons != (%s)"


process :: V.Vector Cons -> V.Vector (VU.Vector ListMap) -> IO Word64
process values args = do
    result <- ioResult

    case result of
        MyLeft err -> error err
        MyRight res -> pure res

    where pars :: V.Vector (Cons, VU.Vector ListMap)
          pars = G.zip values args

          ioResult = G.foldM' lambdaIO MyNothing pars


thunk :: MayEither String Word64 -> (Cons, VU.Vector ListMap) -> IO (MayEither String Word64, (BaldCons, VU.Vector ListMap))

thunk mPredDefs (Promise vow, argMass) = do
    lazyR <- readIORef vow
    let ioStrictR = case lazyR of
            Thunk lazy -> do
                let ordLazy = lazy ()
                writeIORef vow $ Strict ordLazy
                return ordLazy
            Strict strict -> return strict
            
    strictR <- ioStrictR
    return (mPredDefs, (strictR, argMass)) 

thunk mPredDefs (Value role, argMass) = return (mPredDefs, (role, argMass))


calc :: MayEither String Word64 -> (BaldCons, VU.Vector ListMap) -> IO (MayEither String Word64)

calc MyNothing cons@(ConsVoid tag, argMass)
    = pure $ case argMass VU.! pred tag of
        (False, defs, _) -> MyRight defs

        insErr -> MyLeft $ printf err (show insErr)

calc (MyRight predDefs) cons@(ConsVoid tag, argMass)
    = pure $ case argMass VU.! pred tag of
        (False, defs, _) -> MyRight $ predDefs <+> defs

        insErr -> MyLeft $ printf err (show insErr)

calc MyNothing cons@(Cons tag nesTags, argMass)
    = case argMass VU.! pred tag of
        (True, defs, nesArgsIndMass) -> do
            let nesArgMass = nesArgsBD V.! nesArgsIndMass
                nesteds = G.zip nesTags nesArgMass
            
            nes <- G.foldM' lambdaIO MyNothing nesteds
            pure $ fmap (defs<++>) nes

        insErr -> pure $ MyLeft $ printf err (show insErr)

calc (MyRight predDefs) cons@(Cons tag nesTags, argMass)
    = case argMass VU.! pred tag of
        (True, defs, nesArgsIndMass) -> do
            let nesArgMass = nesArgsBD V.! nesArgsIndMass
                nesteds = G.zip nesTags nesArgMass
            
            nes <- G.foldM' lambdaIO MyNothing nesteds
            pure $ fmap (predDefs<->) $ fmap (defs<++>) nes

        insErr -> pure $ MyLeft $ printf err (show insErr)

calc (MyLeft lineErr) _ = pure $ MyLeft lineErr


infixl 1 $.
($.) :: (a -> b) -> a -> b
f $. x = f x

(!!!) :: (Eq k, Show k) => [(k, b)] -> [k] -> [b]
xxs !!! ks
    | Prelude.all isJust res = M.catMaybes res
    | otherwise = error $ printf
        $. "Получены не те ключи.\nИмеющиеся: %s\nПолученные: %s"
        $. (show . fmap fst $ xxs)
        $. (show ks)

    where res = repl ks
          repl [] = []
          repl (ind:inds) = Prelude.lookup ind xxs : repl inds


data InsProm a = InsNesThunk [[Int]] ([Cons] -> a) | InsThunk !a

-- Что равносильно (Left Nothing) + True
values _ = vec [val $ Cons 1 (vec [val $ ConsVoid 1]), val $ ConsVoid 2]
valuesPromise [p1, p2] = vec [prom p1, prom p2]

listIO :: [([Int], InsProm (IO (IORef (Promise BaldCons))))]
listIO =
    [ ([1], InsNesThunk [[1, 1]] $ \[p11] -> thk $ Cons 1 (vec [p11]))
    , ([1,1], InsThunk $ thk $ ConsVoid 1)
    , ([2], InsThunk $ thk $ ConsVoid 2)]
indIO = [[1], [2]] :: [[Int]]

ioList :: [([Int], InsProm (IO (IORef (Promise BaldCons))))] -> IO [([Int], IORef (Promise BaldCons))]
ioList list = traverse (\(key, insIO) -> do {notNes <- lambdaInsIO insIO; pure (key, notNes)}) list

lambdaInsIO :: InsProm (IO (IORef (Promise BaldCons))) -> IO (IORef (Promise BaldCons))
lambdaInsIO insIO = case insIO of
    InsNesThunk inds lambdaIns -> do
        let ioNess = fmap (fmap prom) $ traverse lambdaInsIO $ listIO !!! inds
        argsP <- ioNess
        p <- lambdaIns argsP
        pure p

    InsThunk ins -> do
        p <- ins
        pure p

ioThkMap :: IO (Map [Int] (IORef (Promise BaldCons)))
ioThkMap = ioList listIO >>= \list -> pure $ Map.fromList list

funcValuesPromise :: (Map [Int] (IORef (Promise BaldCons))) -> V.Vector Cons
funcValuesPromise thkMap = if Prelude.all isJust mPList
    then valuesPromise pList
    else error "Получены не те обещания"

    where mPList = fmap (\key -> Map.lookup key thkMap) indIO
          pList = fmap fromJust mPList

funcSim :: V.Vector Cons -> IO String
funcSim ins = do
    p <- process ins argsBD
    pure . (funcBD!!) . firstDef $ p
