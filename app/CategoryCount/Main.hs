{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Either
import Text.Printf


data Void = Void deriving (Show, Eq, Ord, Read)
newtype Id a = Id a deriving (Show, Eq, Ord, Read)

data TypeTriad
    = TypeLeft
    | TypeInside
    | TypeOutside
    | TypeRight
    deriving (Show, Eq, Ord, Read)

data ArgTriad
    = ArgOne TriadEnd
    | ArgTwo TriadEnd TriadEnd
    | ArgThree TriadEnd TriadEnd TriadEnd
    deriving (Show, Eq, Ord, Read)

data Triad a
    = TriadNull
    | TriadOne {arg :: a, dir :: TypeTriad}
    | TriadTwo {arg :: a, dir :: TypeTriad}
    | TriadThree {arg :: a}
    -- | Triad (spontaneity :: Triad, mediation :: Triad, sublation :: Triad)
    deriving (Show, Eq, Ord, Read)

type TriadEnd = Triad Void
type TriadArg = Triad ArgTriad

data a :! b
    = (Maybe a) :! b
    deriving (Show, Eq, Ord, Read)

instance Functor Triad where
    fmap _ TriadNull = TriadNull
    fmap f t = t {arg = f $ arg t}


data Category
    = Нич | Быт | При | Общ
    deriving (Show, Eq, Ord, Read)

data NesCategory
    = Ничего | Мат | Дви | ПроВре | Мех | Хим | Орг | Сил | Отн | Над
    deriving (Show, Eq, Ord, Read)


l = Left
r = Right
n0 = False
n1 = True
--f c a b c = c a b c

combBD :: [TriadEnd]
combBD =
    [ TriadNull
    , TriadOne Void TypeLeft     --(n1, n0, n0)
    , TriadOne Void TypeInside   --(n0, n1, n0)
    , TriadTwo Void TypeLeft     --(n1, n1, n0)
    , TriadOne Void TypeRight    --(n0, n0, n1)
    , TriadTwo Void TypeOutside  --(n1, n0, n1)
    , TriadTwo Void TypeRight    --(n0, n1, n1)
    , TriadThree Void
    ]

natCombBD = tail combBD
contNatCompBD :: [[TriadEnd]]
contNatCompBD = fmap return natCombBD

createArg :: TriadEnd -> [TriadEnd] -> TriadArg
createArg TriadNull _ = TriadNull
createArg t@(TriadOne _ _) [a]       = t {arg = ArgOne a}
createArg t@(TriadTwo _ _) [a, b]    = t {arg = ArgTwo a b}
createArg t@(TriadThree _) [a, b, c] = t {arg = ArgThree a b c}
createArg t arg = error $ printf "createArg: Аргументы не совпадают: %s %s" (show t) (show arg)


createNest :: [TriadEnd] -> [Triad [TriadArg]] 
createNest [] = []
createNest (TriadNull:ts) = TriadNull : createNest ts

createNest (t@(TriadOne _ _):ts)
    = t {arg = fmap (createArg t) contNatCompBD}
    : createNest ts

createNest (t@(TriadTwo _ _):ts)
    = t {arg = fmap (createArg t) (cycleArg 2 contNatCompBD)}
    : createNest ts

createNest (t@(TriadThree _):ts)
    = t {arg = fmap (createArg t) (cycleArg 3 contNatCompBD)}
    : createNest ts


cycleArg :: Int -> [[TriadEnd]] -> [[TriadEnd]]
cycleArg 1 cont = cont
cycleArg n cont = cycleArg (pred n) $ infinityElem cont

    where infinityElem [] = []
          infinityElem (cont:cs)
            = infinityCont natCombBD (repeat cont)
            ++ infinityElem cs

          infinityCont :: [TriadEnd] -> [[TriadEnd]] -> [[TriadEnd]]
          infinityCont [] _ = []
          infinityCont (el:es) (cont:cs)
            = (el : cont)
            : infinityCont es cs


calcNumber :: [Triad [TriadArg]] -> Int -> [Int :! (Triad [Int :! TriadArg])]
calcNumber [] _ = []
calcNumber (TriadNull:ts) num = (Just num) :! TriadNull : calcNumber ts (succ num)
calcNumber (t:ts) num = (Nothing :! newT) : calcNumber ts newNum

    where newNum = num + length (arg t)
          newArg = nesCalc (arg t) num
          newT = t {arg = newArg}
          
          nesCalc :: [TriadArg] -> Int -> [Int :! TriadArg]
          nesCalc [] _ = []
          nesCalc (t:ts) num = (Just num) :! t : nesCalc ts (succ num)


(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b

(+|+) :: String -> String -> String
a +|+ b = a ++ "\n" ++ b

текст = show
q a = "{" ++ a ++ "}"

ls Nothing = ""
ls (Just a) = show a


identCont :: Int :! (Triad [Int :! TriadArg]) -> String
identCont (n :! TriadNull) = ls n ++ q (createStr Нич TriadNull) +++ "="
identCont (_ :! TriadOne arg TypeLeft) = ":: Бытие" +|+ unlines (fmap identCat arg)
identCont (_ :! TriadOne arg TypeInside) = ":: Природа" +|+ unlines (fmap identCat arg)
identCont (_ :! TriadOne arg TypeRight) = ":: Общество" +|+ unlines (fmap identCat arg)
identCont (_ :! TriadTwo arg TypeLeft) = ":: Бытие Природа" +|+ unlines (fmap identCat arg)
identCont (_ :! TriadTwo arg TypeOutside) = ":: Бытие Общество" +|+ unlines (fmap identCat arg)
identCont (_ :! TriadTwo arg TypeRight) = ":: Природа Общество" +|+ unlines (fmap identCat arg)
identCont (_ :! TriadThree arg) = ":: Бытие Природа Общество" +|+ unlines (fmap identCat arg)


identCat :: (Int :! TriadArg) -> String
identCat (n :! TriadNull)                           = ls n ++ q (createStr Нич TriadNull) +++ "="
identCat (n :! TriadOne (ArgOne a) TypeLeft)        = ls n ++ q (createStr Быт a) +++ "="
identCat (n :! TriadOne (ArgOne a) TypeInside)      = ls n ++ q (createStr При a) +++ "="
identCat (n :! TriadOne (ArgOne a) TypeRight)       = ls n ++ q (createStr Общ a) +++ "="
identCat (n :! TriadTwo (ArgTwo a b) TypeLeft)      = ls n ++ q (createStr Быт a) ++ q (createStr При b) +++ "="
identCat (n :! TriadTwo (ArgTwo a b) TypeOutside)   = ls n ++ q (createStr Быт a) ++ q (createStr Общ b) +++ "="
identCat (n :! TriadTwo (ArgTwo a b) TypeRight)     = ls n ++ q (createStr При a) ++ q (createStr Общ b) +++ "="
identCat (n :! TriadThree (ArgThree a b c))    = ls n ++ q (createStr Быт a) ++ q (createStr При b) ++ q (createStr Общ c) +++ "="
identCat t = error $ printf "identCat: Параметры не совпадают: %s" (show t)


createStr :: Category -> TriadEnd -> String
createStr Нич TriadNull                   = текст Ничего

createStr Быт (TriadOne Void TypeLeft)    = текст Мат
createStr Быт (TriadOne Void TypeInside)  = текст Дви
createStr Быт (TriadTwo Void TypeLeft)    = текст Мат +++ текст Дви
createStr Быт (TriadOne Void TypeRight)   = текст ПроВре
createStr Быт (TriadTwo Void TypeOutside) = текст Мат +++ текст ПроВре
createStr Быт (TriadTwo Void TypeRight)   = текст Дви +++ текст ПроВре
createStr Быт (TriadThree Void)           = текст Мат +++ текст Дви +++ текст ПроВре

createStr При (TriadOne Void TypeLeft)    = текст Мех
createStr При (TriadOne Void TypeInside)  = текст Хим
createStr При (TriadTwo Void TypeLeft)    = текст Мех +++ текст Хим
createStr При (TriadOne Void TypeRight)   = текст Орг
createStr При (TriadTwo Void TypeOutside) = текст Мех +++ текст Орг
createStr При (TriadTwo Void TypeRight)   = текст Хим +++ текст Орг
createStr При (TriadThree Void)           = текст Мех +++ текст Хим +++ текст Орг

createStr Общ (TriadOne Void TypeLeft)    = текст Сил
createStr Общ (TriadOne Void TypeInside)  = текст Отн
createStr Общ (TriadTwo Void TypeLeft)    = текст Сил +++ текст Отн
createStr Общ (TriadOne Void TypeRight)   = текст Над
createStr Общ (TriadTwo Void TypeOutside) = текст Сил +++ текст Над
createStr Общ (TriadTwo Void TypeRight)   = текст Отн +++ текст Над
createStr Общ (TriadThree Void)           = текст Сил +++ текст Отн +++ текст Над

createStr c t = error $ printf "createStr: Категории отсутствуют: %s %s" (show c) (show t)


main :: IO ()
main = do
    let allCat = createNest combBD
        allCalc = calcNumber allCat 1
        allText = unlines $ fmap identCont allCalc

    writeFile "./Счёт категорий.txt" allText
