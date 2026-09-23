module Main where

import Data.Either
import Text.Printf


--data Void = Void (Show, Eq, Ord, Read)
data TypeTriad
    = TypeVoid
    | TypeLeft
    | TypeInside
    | TypeOutside
    | TypeRight
    deriving (Show, Eq, Ord, Read)

data ArgTriad
    = ArgVoid
    | ArgOne Triad
    | ArgTwo Triad Triad
    | ArgThree Triad Triad Triad
    deriving (Show, Eq, Ord, Read)

data Triad
    = TriadNull
    | TriadOne {arg :: ArgTriad, dir :: TypeTriad}
    | TriadTwo {arg :: ArgTriad, dir :: TypeTriad}
    | TriadThree {arg :: ArgTriad}
    -- | Triad (spontaneity :: Triad, mediation :: Triad, sublation :: Triad)
    deriving (Show, Eq, Ord, Read)


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

combBD =
    [ TriadNull
    , TriadOne ArgVoid TypeLeft     --(n1, n0, n0)
    , TriadOne ArgVoid TypeInside   --(n0, n1, n0)
    , TriadTwo ArgVoid TypeLeft     --(n1, n1, n0)
    , TriadOne ArgVoid TypeRight    --(n0, n0, n1)
    , TriadTwo ArgVoid TypeOutside  --(n1, n0, n1)
    , TriadTwo ArgVoid TypeRight    --(n0, n1, n1)
    , TriadThree ArgVoid
    ]

natCombBD = tail combBD
contNatCompBD :: [[Triad]]
contNatCompBD = fmap return natCombBD

createArg TriadNull _ = TriadNull
createArg t@(TriadOne _ _) [a]       = t {arg = ArgOne a}
createArg t@(TriadTwo _ _) [a, b]    = t {arg = ArgTwo a b}
createArg t@(TriadThree _) [a, b, c] = t {arg = ArgThree a b c}
createArg t arg = error $ printf "createArg: Аргументы не совпадают: %s %s" (show t) (show arg)


createNest :: [Triad] -> [Triad] 
createNest [] = []
createNest (TriadNull:ts) = TriadNull : createNest ts

createNest (t@(TriadOne _ _):ts)
    = fmap (createArg t) contNatCompBD
    ++ createNest ts

createNest (t@(TriadTwo _ _):ts)
    = fmap (createArg t) (cycleArg 2 contNatCompBD)
    ++ createNest ts

createNest (t@(TriadThree _):ts)
    = fmap (createArg t) (cycleArg 3 contNatCompBD)
    ++ createNest ts


cycleArg :: Int -> [[Triad]] -> [[Triad]]
cycleArg 1 cont = cont
cycleArg n cont = cycleArg (pred n) $ infinityElem cont

    where infinityElem [] = []
          infinityElem (cont:cs)
            = infinityCont natCombBD (repeat cont)
            ++ infinityElem cs

          infinityCont :: [Triad] -> [[Triad]] -> [[Triad]]
          infinityCont [] _ = []
          infinityCont (el:es) (cont:cs)
            = (el : cont)
            : infinityCont es cs


(+++) :: String -> String -> String
a +++ b = a ++ " " ++ b
текст = show
q a = "{" ++ a ++ "}"


identCat :: Triad -> String
identCat TriadNull = q (createString Нич TriadNull) +++ "="
identCat (TriadOne (ArgOne a) TypeLeft)         = q (createString Быт a) +++ "="
identCat (TriadOne (ArgOne a) TypeInside)       = q (createString При a) +++ "="
identCat (TriadOne (ArgOne a) TypeRight)        = q (createString Общ a) +++ "="
identCat (TriadTwo (ArgTwo a b) TypeLeft)       = q (createString Быт a) ++ q (createString При b) +++ "="
identCat (TriadTwo (ArgTwo a b) TypeOutside)    = q (createString Быт a) ++ q (createString Общ b) +++ "="
identCat (TriadTwo (ArgTwo a b) TypeRight)      = q (createString При a) ++ q (createString Общ b) +++ "="
identCat (TriadThree (ArgThree a b c))          = q (createString Быт a) ++ q (createString При b) ++ q (createString Общ c) +++ "="
identCat t = error $ printf "identCat: Параметры не совпадают: %s" (show t)


createString :: Category -> Triad -> String
createString Нич TriadNull = текст Ничего

createString Быт (TriadOne ArgVoid TypeLeft) = текст Мат
createString Быт (TriadOne ArgVoid TypeInside) = текст Дви
createString Быт (TriadTwo ArgVoid TypeLeft) = текст Мат +++ текст Дви
createString Быт (TriadOne ArgVoid TypeRight) = текст ПроВре
createString Быт (TriadTwo ArgVoid TypeOutside) = текст Мат +++ текст ПроВре
createString Быт (TriadTwo ArgVoid TypeRight) = текст Дви +++ текст ПроВре
createString Быт (TriadThree ArgVoid) = текст Мат +++ текст Дви +++ текст ПроВре

createString При (TriadOne ArgVoid TypeLeft) = текст Мех
createString При (TriadOne ArgVoid TypeInside) = текст Хим
createString При (TriadTwo ArgVoid TypeLeft) = текст Мех +++ текст Хим
createString При (TriadOne ArgVoid TypeRight) = текст Орг
createString При (TriadTwo ArgVoid TypeOutside) = текст Мех +++ текст Орг
createString При (TriadTwo ArgVoid TypeRight) = текст Хим +++ текст Орг
createString При (TriadThree ArgVoid) = текст Мех +++ текст Хим +++ текст Орг

createString Общ (TriadOne ArgVoid TypeLeft) = текст Сил
createString Общ (TriadOne ArgVoid TypeInside) = текст Отн
createString Общ (TriadTwo ArgVoid TypeLeft) = текст Сил +++ текст Отн
createString Общ (TriadOne ArgVoid TypeRight) = текст Над
createString Общ (TriadTwo ArgVoid TypeOutside) = текст Сил +++ текст Над
createString Общ (TriadTwo ArgVoid TypeRight) = текст Отн +++ текст Над
createString Общ (TriadThree ArgVoid) = текст Сил +++ текст Отн +++ текст Над

createString c t = error $ printf "createString: Категории отсутствуют: %s %s" (show c) (show t)


addNumber :: Int -> [String] -> [String]
addNumber _ [] = []
addNumber num (x:xs) = (show num ++ x) : addNumber (succ num) xs


main :: IO ()
main = do
    let allCat = createNest combBD
        allText = unlines $ addNumber 1 $ fmap identCat allCat

    writeFile "./Счёт категорий.txt" allText
