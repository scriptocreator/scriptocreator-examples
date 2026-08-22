{-#LANGUAGE LambdaCase, TupleSections, TypeApplications, ScopedTypeVariables, FlexibleContexts #-}

module Main where

--import Data.List.Extra (snoc)
--import Data.Functor.Identity
import Data.Either
import Data.Maybe
import Data.List
import GHC.Unicode
import Control.Monad.State.Lazy
import Data.Semigroup
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Data.Text as R
import qualified Data.Text.IO as RIO
import Control.Exception (try, SomeException)
import Data.Foldable (for_)
import Control.Monad (when)

infixr *:


newtype Чтение word = Чтение {группы :: [Группа word]} deriving (Show, Eq)

data Группа word
    = Группа
        {номер :: Int
        ,подгруппы :: [Подгруппа word]}
    | Группа'
        {подгруппы :: [Подгруппа word]}
    deriving (Show, Eq)

data Подгруппа word -- Словоформа
    = Подгруппа
        {шаблон :: Слово'
        ,ударгруппы :: [Ударная'Группа word]}
    | Подгруппа'
        {ударгруппы :: [Ударная'Группа word]}
    deriving (Show, Eq)

data Ударная'Группа word
    = Ударная'Группа
        {ударение :: Ударение
        ,слова :: word}
    | Ударная'Группа'
        {слова :: word}
    deriving (Show, Eq)


type Ударение = Int

newtype Слово' = Шаблон'Слова [Склад'] deriving (Show, Eq)

data Склад'
    = Шаблон'Одинарный Буква'
    | Шаблон'Двойной Буква' Буква'
    deriving (Show, Eq)

data Буква'
    = Шаблон'Гласной
    | Шаблон'Согласной
    deriving (Show, Eq)


newtype Слово = Слово [Склад] deriving (Show, Eq)

data Склад
    = Одинарный Буква
    | Двойной Буква Буква
    deriving (Show, Eq, Ord)

data Буква
    = Гласная {гласная :: Гласная}
    | Согласная {согласная :: Согласная}
    deriving (Show, Eq, Ord)

data Гласная
    = А | О | У | Ы | Э | Я | Ё | Ю | И | Е
    deriving (Show, Eq, Ord, Enum, Bounded)

data Согласная
    = Б | В | Г | Д | Ж | З | Й | К | Л | М | Н | П | Р | С | Т | Ф | Х | Ц | Ч | Ш | Щ | Ъ | Ь
    deriving (Show, Eq, Ord, Enum, Bounded)

alphabetBD :: [(Char, Буква)]
alphabetBD = [('а', Гласная А), ('о', Гласная О), ('у', Гласная У), ('ы', Гласная Ы), ('э', Гласная Э), ('я', Гласная Я), ('ё', Гласная Ё), ('ю', Гласная Ю), ('и', Гласная И), ('е', Гласная Е), ('б', Согласная Б), ('в', Согласная В), ('г', Согласная Г), ('д', Согласная Д), ('ж', Согласная Ж), ('з', Согласная З), ('й', Согласная Й), ('к', Согласная К), ('л', Согласная Л), ('м', Согласная М), ('н', Согласная Н), ('п', Согласная П), ('р', Согласная Р), ('с', Согласная С), ('т', Согласная Т), ('ф', Согласная Ф), ('х', Согласная Х), ('ц', Согласная Ц), ('ч', Согласная Ч), ('ш', Согласная Ш), ('щ', Согласная Щ), ('ъ', Согласная Ъ), ('ь', Согласная Ь)]

hareSystem :: [Буква] -> [Склад]
hareSystem [] = []
hareSystem (согл@(Согласная _):гл@(Гласная _):ws) = Двойной согл гл : hareSystem ws
hareSystem (соглOne@(Согласная _):соглTwo@(Согласная Ь):ws) = Двойной соглOne соглTwo : hareSystem ws
hareSystem (соглOne@(Согласная _):соглTwo@(Согласная Ъ):ws) = Двойной соглOne соглTwo : hareSystem ws
hareSystem (согл@(Согласная _):ws) = Одинарный согл : hareSystem ws
hareSystem (гл@(Гласная _):ws) = Одинарный гл : hareSystem ws

wordform :: [Склад] -> [Склад']
wordform [] = []
wordform (Двойной lattOne lattTwo:ws) = Шаблон'Двойной (wordformLetter lattOne) (wordformLetter lattTwo) : wordform ws
wordform (Одинарный latt:ws) = Шаблон'Одинарный (wordformLetter latt) : wordform ws

wordformLetter (Согласная _) = Шаблон'Согласной
wordformLetter (Гласная _) = Шаблон'Гласной

charInType :: Char -> Maybe Буква
charInType curChar = pure snd <*> flip find alphabetBD (\case
    (char, letter)
        | curChar == char -> True
        | otherwise -> False)

typeInChar :: Буква -> Maybe Char
typeInChar curLetter = pure fst <*> flip find alphabetBD (\case
    (char, letter)
        | curLetter == letter -> True
        | otherwise -> False)

calcVowelStrike :: String -> Maybe Int
calcVowelStrike = nesVowelStrike 1

    where nesVowelStrike _ [] = Nothing
          nesVowelStrike calc (l:'\769':ls)
            | isJust mTypeL && isVowel typeL = return calc
            | otherwise = error ("Вместо гласной, под ударением "++quote (show calc)++" символ: "++quote [l])

            where mTypeL = charInType l
                  typeL = fromJust mTypeL

          nesVowelStrike calc (l:ls)
            | l == 'ё' || l == 'Ё' = return calc
            | isJust mTypeL && isVowel typeL = nesVowelStrike futCalc ls
            | otherwise = nesVowelStrike calc ls

            where futCalc = succ calc
                  mTypeL = charInType l
                  typeL = fromJust mTypeL

setVowelStrike :: Int -> String -> String
setVowelStrike num word = nesVowelStrike 1 word

    where nesVowelStrike _ [] = error ("Не обнаружено нужной ударной гласной "++quote (show num)++", в слове: "++quote word)
          nesVowelStrike calc (l:ls)
            | calc > num = error ("Нужная гласная "++quote (show num)++ " под ударение не найдена, в слове: "++quote word)
            | num == calc && isJust mTypeL && isVowel typeL = newLetter ++ ls
            | isJust mTypeL && isVowel typeL = l : nesVowelStrike futCalc ls
            | otherwise = l : nesVowelStrike calc ls

            where mTypeL = charInType l
                  typeL = fromJust mTypeL
                  futCalc = succ calc
                  newLetter
                    | l == 'ё' || l == 'Ё' = [l]
                    | otherwise = [l, '\769']

isVowel (Гласная _) = True
isVowel _ = False

isConsonant (Согласная _) = True
isConsonant _ = False

deTypingWord :: (String -> String) -> [Склад] -> String
deTypingWord f [] = []

deTypingWord f (Двойной one two:ls)
    | all isJust listMLetts = f listLetts ++ deTypingWord f ls
    | otherwise = error "Невозможная ситуация, букв склада не существует"

    where listMLetts = [typeInChar one, typeInChar two]
          listLetts :: String
          listLetts = fmap fromJust listMLetts

deTypingWord f (Одинарный one:ls)
    | isJust mLett = f [lett] ++ deTypingWord f ls
    | otherwise = error "Невозможная ситуация, букв склада не существует"

    where mLett = typeInChar one
          lett = fromJust mLett

quote line = "«"++line++"»"

lengthReading :: Слово -> Int
lengthReading (Слово word) = sum $ fmap fromEnum word


filterLevelVowel :: (Int -> Int -> Bool) -> Int -> Слово -> Bool
filterLevelVowel (##) num (Слово list) = nestedVowel list

    where nestedVowel [] = True
          nestedVowel (x:xs) = fmapSyllable lamLetter x && nestedVowel xs

          lamLetter listLett = all ((##num) . fromEnum . гласная) (filter isVowel listLett)


elemLetter :: Буква -> Слово -> Bool
elemLetter letter (Слово list) = nestedVowel list

    where nestedVowel [] = False
          nestedVowel (x:xs) = fmapSyllable lamLetter x || nestedVowel xs

          lamLetter listLett = letter `elem` filter isVowel listLett


fmapSyllable :: ([Буква] -> b) -> Склад -> b
fmapSyllable f (Одинарный a) = f [a]
fmapSyllable f (Двойной a b) = f [a, b]


eqTemplWord :: Слово' -> Слово -> Bool
eqTemplWord (Шаблон'Слова templ) (Слово word) = all nestedEq (zip templ word)

    where nestedEq (Шаблон'Одинарный oneA, Одинарный twoA) = nestedLet oneA twoA
          nestedEq (Шаблон'Двойной oneA oneB, Двойной twoA twoB) = nestedLet oneA twoA && nestedLet oneB twoB
          nestedEq _ = False

          nestedLet Шаблон'Гласной (Гласная _) = True
          nestedLet Шаблон'Согласной (Согласная _) = True
          nestedLet _ _ = False


eqReader :: Чтение [Слово] -> [(Слово', Слово)]
eqReader (Чтение reader) = concatMap (catMaybes . eqGroup) reader

eqGroup :: Группа [Слово] -> [Maybe (Слово', Слово)]
eqGroup group = concatMap (filter isJust . eqSubgroup) (подгруппы group)

eqSubgroup :: Подгруппа [Слово] -> [Maybe (Слово', Слово)]
eqSubgroup (Подгруппа templ subgroup) = concatMap (filter isJust . eqStrikeGroup templ) subgroup
eqSubgroup _ = return Nothing

eqStrikeGroup :: Слово' -> Ударная'Группа [Слово] -> [Maybe (Слово', Слово)]
eqStrikeGroup templ strikeGroup
    | null errors = return Nothing
    | otherwise = errors

    where words = слова strikeGroup
          zipper = flip zip words $ fmap (eqTemplWord templ) words
          errors = fmap (Just . (templ,) . snd) (filter (not . fst) zipper)


showTemplate :: Слово' -> String
showTemplate (Шаблон'Слова word) = init $ concatMap nestedSyll word

    where nestedSyll (Шаблон'Одинарный a) = nestedLett a ++ "|"
          nestedSyll (Шаблон'Двойной a b) = nestedLett a ++ " " ++ nestedLett b ++ "|"
          
          nestedLett Шаблон'Гласной = "Гл"
          nestedLett Шаблон'Согласной = "Согл"



instance Enum Склад where
    fromEnum (Одинарный _) = 1
    fromEnum (Двойной _ _) = 2


instance Functor Чтение where
    fmap f (Чтение группы) = Чтение $ fmap (fmap f) группы

instance Functor Группа where
    fmap f (Группа номер подгруппы) = Группа номер $ fmap (fmap f) подгруппы
    fmap f (Группа' подгруппы) = Группа' $ fmap (fmap f) подгруппы

instance Functor Подгруппа where
    fmap f (Подгруппа шаблон ударгруппы) = Подгруппа шаблон $ fmap (fmap f) ударгруппы
    fmap f (Подгруппа' ударгруппы) = Подгруппа' $ fmap (fmap f) ударгруппы

instance Functor Ударная'Группа where
    fmap f (Ударная'Группа ударение слова) = Ударная'Группа ударение $ f слова
    fmap f (Ударная'Группа' слова) = Ударная'Группа' $ f слова


instance Applicative Чтение where
    pure a = Чтение [pure a]

    codeЧтение <*> dataЧтение
        = effectMonad funcs $ \f -> fmap f dataЧтение

         where funcs = getListTree codeЧтение

instance Applicative Группа where
    pure a = Группа' [pure a]

    codeГруппа <*> dataГруппа
        = effectMonad funcs $ \f -> fmap f dataГруппа

            where funcs = getListTree codeГруппа

instance Applicative Подгруппа where
    pure a = Подгруппа' [pure a]

    codeПодгруппа <*> dataПодгруппа
        = effectMonad funcs $ \f -> fmap f dataПодгруппа

            where funcs = getListTree codeПодгруппа

instance Applicative Ударная'Группа where
    pure = Ударная'Группа'

    codeУдаргруппа <*> dataУдаргруппа
        = fmap func dataУдаргруппа

            where func = слова codeУдаргруппа

--(<&*>) :: [a -> b] -> a -> b
--[] <&*> el = el
--(f:fs) <&*> el = fs <&*> f el

effectMonad :: [a -> b] -> ((a -> b) -> m b) -> m b
effectMonad funcs lambda = case funcs of
    [] -> error "Нарушено правило монады: функция не была передана"
    [f] -> lambda f
    (_:_) -> error "Превышены ограничения монады: получен список функций"


class GetListTree f where
    getListTree :: f (a -> b) -> [a -> b]

instance GetListTree Чтение where
    getListTree (Чтение reading)
        = concatMap getListTree reading

instance GetListTree Группа where
    getListTree group =
        let listSubgrops = подгруппы group
        in concatMap getListTree listSubgrops

instance GetListTree Подгруппа where
    getListTree subgroup =
        let listStrikeGroups = ударгруппы subgroup
        in fmap слова listStrikeGroups


class ДеклЧтение a where
    createGroup         :: Группа [a]           -> Чтение [a] -> Чтение [a]
    createSubgroup      :: Подгруппа [a]        -> Чтение [a] -> Чтение [a]
    createStrikeGroup   :: Ударная'Группа [a]   -> Чтение [a] -> Чтение [a]
    createWord          :: a                    -> Чтение [a] -> Чтение [a]

instance ДеклЧтение Слово where
    createGroup group (Чтение группы) = Чтение $ группы ++ [group]

    createSubgroup subgroup (Чтение []) = чтение *: группа' [subgroup]
    createSubgroup subgroup (Чтение группы) =
        let prevГруппы = init группы
            curГруппа = last группы
            newГруппа = curГруппа {подгруппы = подгруппы curГруппа ++: subgroup}

        in Чтение $ prevГруппы ++: newГруппа

    createStrikeGroup strikeGroup (Чтение []) = чтение *: группа' *: подгруппа' [strikeGroup]
    createStrikeGroup strikeGroup (Чтение группы)
        | null allПодгруппы =
            let newГруппа = curГруппа {подгруппы = [подгруппа' *: strikeGroup]}

            in Чтение $ prevГруппы ++: newГруппа

        | otherwise =
            let newПодгруппа = сurПодгруппа {ударгруппы = ударгруппы сurПодгруппа ++: strikeGroup}
                newГруппа = curГруппа {подгруппы = prevПодгруппы ++: newПодгруппа}

            in Чтение $ prevГруппы ++: newГруппа

        where prevГруппы = init группы
              curГруппа = last группы

              allПодгруппы = подгруппы curГруппа
              prevПодгруппы = init allПодгруппы
              сurПодгруппа = last allПодгруппы

    createWord word (Чтение []) = чтение *: группа' *: подгруппа' *: ударгруппа' [word]
    createWord word (Чтение группы)
        | null allПодгруппы =
            let newГруппа = curГруппа {подгруппы = [подгруппа' *: ударгруппа' [word]]}

            in Чтение $ prevГруппы ++: newГруппа

        | null allУдаргруппы =
            let newПодгруппа = сurПодгруппа {ударгруппы = [ударгруппа' [word]]}
                newГруппа = curГруппа {подгруппы = prevПодгруппы ++: newПодгруппа}

            in Чтение $ prevГруппы ++: newГруппа

        | otherwise =
            let newУдаргруппа = fmap (++:word) сurУдаргруппа
                newПодгруппа = сurПодгруппа {ударгруппы = prevУдаргруппы ++: newУдаргруппа}
                newГруппа = curГруппа {подгруппы = prevПодгруппы ++: newПодгруппа}

            in Чтение $ prevГруппы ++: newГруппа

        where prevГруппы = init группы
              curГруппа = last группы

              allПодгруппы = подгруппы curГруппа
              prevПодгруппы = init allПодгруппы
              сurПодгруппа = last allПодгруппы

              allУдаргруппы = ударгруппы сurПодгруппа
              prevУдаргруппы = init allУдаргруппы
              сurУдаргруппа = last allУдаргруппы


чтение = Чтение
группа' = Группа'
подгруппа' = Подгруппа'
ударгруппа' = Ударная'Группа'

группа = Группа
подгруппа = Подгруппа
ударгруппа = Ударная'Группа

a *: b = a [b]
a ++: b = a ++ [b]


data Pair = Start | End deriving Show

data Markdown
    = MarkVoid

    | ThinkUnderlined Pair
    | ThinkFat

    | Underlined -- Подчёркнутый
    | Fat -- Жирный

    | ThinkComp Markdown [Markdown]
    | Comp Markdown [Markdown]
    deriving Show


class DeclMarkdown a where
    setUnder :: Pair -> State a ()
    setFat :: State a ()
    set :: a -> a -> a
    reverseDeep :: a -> a
    elemThink :: a -> a -> Bool


instance DeclMarkdown Markdown where
    setUnder pair = do
        md <- get
        put $ set (ThinkUnderlined pair) md

    setFat = do
        md <- get
        put $ set ThinkFat md

    -- `set` берёт структуру в сортировке от пройденного до актуального
    set think (Comp MarkVoid []) = Comp MarkVoid [think]
    set think (Comp MarkVoid calcul)
        | logStrict lastCalcul = Comp MarkVoid $ calcul ++: think
        | logThink lastCalcul = Comp MarkVoid newCalcul

        where lastCalcul = last calcul
              initCalcul = init calcul

              deepSet :: Markdown -> Markdown

              deepSet curUnder@(ThinkUnderlined pair)
                | curUnder == think = Underlined
                | otherwise = thinkComp curUnder *: think

              deepSet ThinkFat
                | ThinkFat == think = Fat
                | otherwise = thinkComp ThinkFat *: think

              deepSet (ThinkComp curMk [])
                | curMk == think = thinkInStrict curMk
                | otherwise = thinkComp curMk *: think

              deepSet (ThinkComp curMk deepMks)
                | curMk == think = comp strictCurMk newMks
                | otherwise = thinkComp curMk futureSet

                where initMks = init deepMks
                      lastMk = last deepMks

                      strictCurMk = thinkInStrict curMk
                      mNewLastMk = thinkCompInFeed lastMk
                      newMks
                        | isRight mNewLastMk =
                            let correctMd = fromRight MarkVoid mNewLastMk
                            in initMks ++: correctMd
                        | otherwise =
                            let transMd = fromLeft [MarkVoid] mNewLastMk
                            in initMks ++ transMd

                      futureSet
                        | logThink lastMk = initMks ++: deepSet lastMk
                        | otherwise = deepMks ++: think

              deepSet (Comp mk mks) = error ("Непрвильное состояние deepSet, think: "++quote (show think)
                                             ++", mk: "++quote (show mk)++", mks: "++quote (show mks))

              result = deepSet lastCalcul
              newCalcul :: [Markdown]
              newCalcul = initCalcul ++: result


    reverseDeep (ThinkComp mk mks) = ThinkComp mk deepNewMks

        where presentNewMks = reverse mks
              deepNewMks = fmap reverseDeep presentNewMks

    reverseDeep (Comp mk mks) = Comp mk deepNewMks

        where presentNewMks = reverse mks
              deepNewMks = fmap reverseDeep presentNewMks

    reverseDeep mk = mk


    elemThink _ (Comp _ []) = False
    elemThink _ (ThinkComp _ []) = False

    elemThink el (Comp MarkVoid mks)
        = elemThink el lastMk

        where lastMk = last mks

    elemThink el (ThinkComp mk mks)
        | el == mk = True
        | otherwise = elemThink el lastMk

        where lastMk = last mks

    elemThink el mk
        | el == mk = True
        | otherwise = False


instance Eq Markdown where
    MarkVoid == MarkVoid = True

    ThinkUnderlined Start == ThinkUnderlined End = True
    ThinkFat == ThinkFat = True

    Underlined == Underlined = True
    Fat == Fat = True

    (ThinkComp mdOne _) == (ThinkComp mdTwo _) = mdOne == mdTwo

    mdOne == (ThinkComp mdTwo _) = mdOne == mdTwo
    (ThinkComp mdOne _) == mdTwo = mdOne == mdTwo

    (Comp mdOne _) == (Comp mdTwo _) = mdOne == mdTwo

    mdOne == (Comp mdTwo _) = mdOne == mdTwo
    (Comp mdOne _) == mdTwo = mdOne == mdTwo

    _ == _ = False

    mdOne /= mdTwo = not (mdOne == mdTwo)


logThink :: Markdown -> Bool
logThink (ThinkUnderlined _) = True
logThink ThinkFat = True
logThink (ThinkComp think _) = logThink think
logThink _ = False

logStrict :: Markdown -> Bool
logStrict Underlined = True
logStrict Fat = True
logStrict (Comp strict _) = logStrict strict
logStrict _ = False

thinkComp = ThinkComp
comp = Comp


thinkInStrict :: Markdown -> Markdown
thinkInStrict (ThinkUnderlined _) = Underlined
thinkInStrict ThinkFat = Fat
thinkInStrict (ThinkComp think md) = flip comp md $ thinkInStrict think
thinkInStrict md = md
{-
-- Игнорирует `Comp`, поскольку просто выравнивает граф обещаний
thinkGraphInLinear :: Markdown -> [Markdown]
thinkGraphInLinear (ThinkComp think md) = think : thinkGraphInLinear md
thinkGraphInLinear md = [md]
-}

templateInDeepFeed :: Markdown -> [Markdown]
templateInDeepFeed (Comp MarkVoid md) = md
templateInDeepFeed md = [md]

-- Подразумевает применение на готовой структуре
deleteDeepFeedVoid :: [Markdown] -> [Markdown]
deleteDeepFeedVoid [] = []
deleteDeepFeedVoid (MarkVoid:mks) = deleteDeepFeedVoid mks

deleteDeepFeedVoid (Comp mk deepMks:feedMks)
    = Comp mk (deleteDeepFeedVoid deepMks)
    : deleteDeepFeedVoid feedMks

deleteDeepFeedVoid (ThinkComp mk deepMks:feedMks)
    = ThinkComp mk (deleteDeepFeedVoid deepMks)
    : deleteDeepFeedVoid feedMks

deleteDeepFeedVoid (mk:mks) = mk : deleteDeepFeedVoid mks


feedGraphInLinear :: [Markdown] -> [Markdown]
feedGraphInLinear [] = []
feedGraphInLinear (ThinkComp mk deepMks:feedMks) = mk : feedGraphInLinear deepMks ++ feedGraphInLinear feedMks
feedGraphInLinear (Comp mk deepMks:feedMks) = mk : feedGraphInLinear deepMks ++ feedGraphInLinear feedMks
feedGraphInLinear (mk:mks) = mk : feedGraphInLinear mks


thinkCompInFeed :: Markdown -> Either [Markdown] Markdown
thinkCompInFeed void@(Comp MarkVoid []) = return void
thinkCompInFeed (Comp MarkVoid mks) = return $ comp MarkVoid newMks

    where initMks = init mks
          lastMk = last mks
          mNewLastMk = thinkCompInFeed lastMk
          newMks
            | isRight mNewLastMk =
                let correctMd = fromRight MarkVoid mNewLastMk
                in initMks ++: correctMd
            | otherwise =
                let transMd = fromLeft [MarkVoid] mNewLastMk
                in initMks ++ transMd

thinkCompInFeed (ThinkComp mk mks) = Left $ mk : newMks

    where initMks = init mks
          lastMk = last mks
          mNewLastMk = thinkCompInFeed lastMk
          newMks
            | isRight mNewLastMk =
                let correctMd = fromRight MarkVoid mNewLastMk
                in initMks ++: correctMd
            | otherwise =
                let transMd = fromLeft [MarkVoid] mNewLastMk
                in initMks ++ transMd

thinkCompInFeed mk = return mk

thinkFeed md = case thinkCompInFeed md of
    Right (Comp MarkVoid mks) -> comp MarkVoid mks
    err -> error ("При выравнивании обещаний, получен не тот тип: "++quote (show err))


logUnder Underlined = True
logUnder (ThinkComp mk _) = logUnder mk
logUnder (Comp mk _) = logUnder mk
logUnder _ = False

logFat Fat = True
logFat (ThinkComp mk _) = logFat mk
logFat (Comp mk _) = logFat mk
logFat _ = False

logUnit (Comp _ _) = False
logUnit (ThinkComp _ _) = False
logUnit _ = True

logComp (Comp _ _) = True
logComp (ThinkComp _ _) = True
logComp _ = False

logDeepFeed (Comp _ _) = True
logDeepFeed _ = False

--nestedFeed (ThinkComp _ feed) = feed
nestedFeed (Comp _ feed) = feed

logClosureLists [] _ = True
logClosureLists _ [] = True
logClosureLists (a:as) (b:bs)
    | a == b = logClosureLists as bs
    | otherwise = False

end el = foldr (:) [el]

setEndEl x = modify (end x)
setEndList xs = modify (++xs)

startUnder = ['<','u','>']
endUnder = ['<','/','u','>']
fat = ['*','*']
startFat = ['<','b','>']
endFat =['<','/','b','>']

markInLog (ThinkUnderlined _) = LogUnder
markInLog ThinkFat = LogFat
markInLog Underlined = LogUnder
markInLog Fat = LogFat
markInLog (ThinkComp mk _) = markInLog mk
markInLog (Comp mk _) = markInLog mk
markInLog _ = LogMind

fPureLog el = (==el)

data LogMarkdown = LogMind | LogUnder | LogFat deriving (Show, Eq, Ord)
data LogWord = LogSubgroup | LogStrikeGroup | LogOrdinary deriving (Show, Eq, Ord)

-- Моя личная функция, нужна для анализа моего форматирования
markInWord :: [Markdown] -> LogWord
markInWord [] = LogOrdinary
markInWord mks
    | any logUnder stripMks && any logFat stripMks = LogSubgroup
    | any logFat stripMks = LogStrikeGroup
    | otherwise = error $ show mks

    where stripMks = feedGraphInLinear mks


sCreateGroup el = modify (createGroup el)
sCreateSubgroup el = modify (createSubgroup el)
sCreateStrikeGroup el = modify (createStrikeGroup el)
sCreateWord el = modify (createWord el)

initMD = Comp MarkVoid []



markdownClean :: (LogMarkdown -> Bool) -> [Markdown] -> String -> State String ([Markdown], String)
markdownClean fPure mmks [] = return (mmks, [])

markdownClean fPure [] xxs
    | fPure LogMind = setEndList xxs >> return ([], [])
    | otherwise = case xxs of
        ('<':'/':'u':'>':us) | fPure LogUnder -> return ([], us)
        ('*':'*':fs)         | fPure LogFat   -> return ([], fs)
        (x:xs) -> do setEndEl x
                     markdownClean fPure [] xs

markdownClean fPure mmks@(headMks:mks) ('<':'u':'>':us)
    | logUnder headMks = if not $ logDeepFeed headMks
        then markdownClean fPure mmks us
        else do setEndList stateWord
                markdownClean fPure dregsMks dregsUs
    | otherwise = do
        setEndList startUnder
        markdownClean fPure mmks us

    where Comp el deep = headMks
          deepFPure = fPureLog $ markInLog el
          (returnContext, stateWord) = flip runState "" $ markdownClean deepFPure deep us
          (dregsMks, dregsUs) = returnContext

markdownClean fPure mmks@(headMks:mks) ('<':'/':'u':'>':us)
    | logUnder headMks = markdownClean fPure mks us
    | otherwise = do
        setEndList endUnder
        markdownClean fPure mmks us

markdownClean fPure mmks@(headMks:mks) ('<':'b':'>':us)
    | logFat headMks = if not $ logDeepFeed headMks
        then markdownClean fPure mmks us
        else do setEndList stateWord
                markdownClean fPure dregsMks dregsUs
    | otherwise = do
        setEndList startFat
        markdownClean fPure mmks us

    where Comp el deep = headMks
          deepFPure = fPureLog $ markInLog el
          (returnContext, stateWord) = flip runState "" $ markdownClean deepFPure deep us
          (dregsMks, dregsUs) = returnContext

markdownClean fPure mmks@(headMks:mks) ('<':'/':'b':'>':us)
    | logFat headMks = markdownClean fPure mks us
    | otherwise = do
        setEndList endFat
        markdownClean fPure mmks us

markdownClean fPure mmks@(headMks:mks) ('*':'*':fs)
    | logFat headMks = if not $ logDeepFeed headMks
        then markdownClean fPure mmks fs
        else do setEndList stateWord
                markdownClean fPure dregsMks dregsUs
    | otherwise = do
        setEndList fat
        markdownClean fPure mmks fs

    where Comp el deep = headMks
          deepFPure = fPureLog $ markInLog el
          (returnContext, stateWord) = flip runState "" $ markdownClean deepFPure deep fs
          (dregsMks, dregsUs) = returnContext

markdownClean fPure mmks (x:xs) = setEndEl x >> markdownClean fPure mmks xs


searchClean _ [] = []
searchClean char (x:xs)
    | char == x = xs
    | otherwise = x : searchClean char xs


-- Функция которая возвращает список форматирования слова (DSL)
markdownMarker :: Markdown -> String -> State Markdown String
markdownMarker _ [] = return ""

markdownMarker contChain ('<':'u':'>':us) = do
    setUnder Start
    updContChain <- get
    returnLine <- markdownMarker updContChain us
    updContChain <- get
    markdownMarker updContChain returnLine

markdownMarker contChain ('<':'/':'u':'>':us)
    | ThinkUnderlined End `elemThink` contChain = setUnder End >> return us
    | otherwise = do
        setUnder End
        updContChain <- get
        markdownMarker updContChain us

markdownMarker contChain ('<':'b':'>':us) = do
    setFat
    updContChain <- get
    returnLine <- markdownMarker updContChain us
    updContChain <- get
    markdownMarker updContChain returnLine

markdownMarker contChain ('<':'/':'b':'>':us)
    | ThinkFat `elemThink` contChain = setFat >> return us
    | otherwise = do
        setFat
        updContChain <- get
        markdownMarker updContChain us

markdownMarker contChain ('*':'*':fs)
    | ThinkFat `elemThink` contChain = setFat >> return fs
    | otherwise = do
        setFat
        updContChain <- get
        returnLine <- markdownMarker updContChain fs
        updContChain <- get
        markdownMarker updContChain returnLine

markdownMarker contChain (_:xs) = markdownMarker contChain xs


data LogList = New | Old deriving (Show, Eq, Ord)
data LogListPar a = New' {par :: a} | Old' deriving (Show, Eq)

graphInList :: Чтение [Слово] -> String
graphInList (Чтение reading) = intercalate "\n\n\n" nesList

    where nesList = filter (not . null) $
            fmap groupInLine reading

groupInLine :: Группа [Слово] -> String
groupInLine group
    | null allПодгруппы || null nesList = ""
    | otherwise = case group of
        Группа' _ -> ("### Группа\n\n"++) $ intercalate "\n\n" nesList
        Группа num _ ->
            let lineGroup = "### Группа №"++show num++"\n"
                resultList = lineGroup : nesList

            in unlines resultList

    where allПодгруппы = подгруппы group
          nesList = filter (not . null) $
            fmap subgroupInLine allПодгруппы

subgroupInLine :: Подгруппа [Слово] -> String
subgroupInLine subgroup
    | null allУдаргруппы || null nesList = ""
    | otherwise = unlines nesList

    where allУдаргруппы = ударгруппы subgroup
          headУдаргруппы = head allУдаргруппы
          tailУдаргруппы = tail allУдаргруппы

          nesList = filter (not . null) $
            strikeGroupInLine (New, Old) headУдаргруппы : fmap (strikeGroupInLine (Old, Old)) tailУдаргруппы

strikeGroupInLine :: (LogList, LogList) -> Ударная'Группа [Слово] -> String
strikeGroupInLine (conNewS, conOldS) strikeGroup
    | null pureAllСлова || null nesList = ""
    | otherwise = unlines nesList

    where allСлова = слова strikeGroup
          pureAllСлова = flip filter allСлова $ \case {(Слово []) -> False; _ -> True}
          headСлово = head pureAllСлова
          tailСлова = tail pureAllСлова
          nesList = filter (not . null) $
            case strikeGroup of
                Ударная'Группа' _ -> lamNesList Old'
                Ударная'Группа strike _ -> lamNesList $ New' strike

          lamNesList curCon = typeWordInLine (conNewS, curCon) headСлово : fmap (typeWordInLine (conOldS, Old')) tailСлова

typeWordInLine :: (LogList, LogListPar Int) -> Слово -> String
typeWordInLine cont (Слово typeWord)
    | null typeWord = ""
    | otherwise = case cont of
        (New, New' _) -> let markWord = setVowelStrike strike $ init $ flip deTypingWord typeWord $ \s -> s ++: '|'
                         in "**<u>"++markWord++"</u>**"

        (New, Old') -> let excWord = init $ flip deTypingWord typeWord $ \s -> s ++: '|'
                       in "**<u>"++excWord++"</u>**"

        (Old, New' _) -> let strikeWord = setVowelStrike strike $ deTypingWord id typeWord
                         in "**"++strikeWord++"**"

        (Old, Old') -> let ordinWord = deTypingWord id typeWord
                       in ordinWord

        -- _ -> error ("Невозможная ситуация с обработкой слова: "++show cont)

    where strike = par $ snd cont



listInGraph :: [String] -> State (Чтение [Слово]) ()
listInGraph [] = return ()
listInGraph ("":ws) = listInGraph ws
listInGraph (w@(' ':' ':' ':' ':'Г':'р':'у':'п':'п':'а':lineNum):ws) = do
    let num :: Int
        num = read $ filter ((==DecimalNumber) . generalCategory) lineNum
    case lineNum of
        [] -> sCreateGroup $ группа' []
        (' ':'№':_) -> sCreateGroup $ группа num []
    listInGraph ws

listInGraph (word:ws)
    | all ((==DashPunctuation) . generalCategory) word = listInGraph ws
    | all ((==OtherPunctuation) . generalCategory) word = listInGraph ws
    | all ((==Space) . generalCategory) word = listInGraph ws
    | otherwise = case typeMksWord of
        LogSubgroup     -> sCreateSubgroup newSubgroup       >> listInGraph ws
        LogStrikeGroup  -> sCreateStrikeGroup newStrikeGroup >> listInGraph ws
        LogOrdinary     -> sCreateWord typeWord              >> listInGraph ws

    where --pureWord = filter (/='|') word
          mDirtMksWord = execState (markdownMarker initMD word) initMD
          mksWord = deleteDeepFeedVoid $ templateInDeepFeed $ reverseDeep $ thinkFeed mDirtMksWord
          typeMksWord = markInWord mksWord
          pureWord = flip execState "" $ markdownClean (fPureLog LogMind) mksWord word

          pureTypeWord = hareSystem $ mapMaybe charInType pureWord
          typeWord = Слово pureTypeWord
          typeWordform = Шаблон'Слова $ wordform pureTypeWord
          mStrike = calcVowelStrike pureWord
          strike = fromJust mStrike

          newУдаргруппа
            | isJust mStrike = ударгруппа strike
            | otherwise = ударгруппа'
          newStrikeGroup = newУдаргруппа [typeWord]
          newSubgroup = подгруппа typeWordform *: newStrikeGroup


runReading :: Args [String] -> State (Чтение [Слово]) ()
runReading ArgsVoid = return ()
runReading (ArgsReading file) = listInGraph file
runReading (ArgsGroup num file futArgs) = do
    sCreateGroup $ группа num []
    listInGraph file
    runReading futArgs



safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path = do
  result <- try @SomeException (RIO.readFile path)
  pure $ either (const Nothing) (Just . R.unpack) result

doWhileFold :: (a -> a) -> (a -> b -> c) -> (a -> Bool) -> a -> [b] -> [c]
doWhileFold transCalc create log = go

  where go _ [] = []
        go calc (x:xs)  | log calc  = [newEl]
                        | otherwise = newEl : go newCalc xs
                        where newEl = create calc x
                              newCalc = transCalc calc


data ModeReader = ReaderGroups | ReaderReading deriving (Show, Eq, Ord, Read)
data ModeWriting = WritingAll  | WritingInitial | WritingCategory | WritingCombinations deriving (Show, Eq, Ord, Read)
data Args a = ArgsVoid | ArgsGroup Int a (Args a) | ArgsReading a deriving (Show, Eq)

logWriting WritingAll = "./Все.md"
logWriting WritingInitial = "./Начальная.md"
logWriting WritingCategory = "./Категории.md"
logWriting WritingCombinations = "./Комбинации.md"

main :: IO ()
main = do
    dirtListAgs <- getArgs

    let (modeWriting, chainArgs :: Args String) = case dirtListAgs of
            [] -> error "Ничего не получено"
            [_] -> error "Данных слишком мало"

            (modeReader:modeWriting:list) ->
                let (mReader, mWriting) = (readMaybe modeReader, readMaybe modeWriting) :: (Maybe ModeReader, Maybe ModeWriting)
                    (reader, writing) = (fromJust mReader, fromJust mWriting)

                in case (isJust mReader, isJust mWriting) of
                    (False, False)  -> error "Режим чтения и записи неправильные"
                    (False, True)   -> error "Не получен режим чтения"
                    (True, False)   -> error "Не получен режим ввода"
                    (True, True)    -> (writing, case reader of
                        ReaderGroups  -> funcGroupArgs list
                        ReaderReading -> funcReadArgs list)

            (_:_) -> error "Данных нехватает"

        chainFileConts :: (Args [String] -> Args [String]) -> Args String -> IO (Args [String])
        chainFileConts lamArgs ArgsVoid = return $ lamArgs ArgsVoid

        chainFileConts lamArgs (ArgsReading urlFile) = do
            mFile <- safeReadFile urlFile
            let file = fromJust mFile
                endArgs = lamArgs $ ArgsReading $ lines file
            if isNothing mFile
                then error ("Файла "++urlFile++" Не существует")
                else return endArgs

        chainFileConts lamArgs (ArgsGroup num urlFile futDirtArgs) = do
            mFile <- safeReadFile urlFile
            let file = fromJust mFile
                newArgs = \futArgs -> lamArgs $ ArgsGroup num (lines file) futArgs
            if isNothing mFile
                then error ("Файла "++urlFile++" Не существует")
                else chainFileConts newArgs futDirtArgs

    chainFiles <- chainFileConts id chainArgs

    let graph :: Чтение [Слово]
        graph = flip execState (Чтение []) $ runReading chainFiles
        eqGraph = eqReader graph

        handlerGraph = case modeWriting of
            WritingAll   -> graphInList graph
            WritingInitial -> graphInList filterGraph
            WritingCategory -> intercalate "\n\n\n"
                $ doWhileFold succ lamCat (==fromEnum (maxBound :: Гласная)) 0 (repeat filterGraph)
            WritingCombinations -> intercalate "\n\n\n"
                $ doWhileFold succ lamComb (==fromEnum (maxBound :: Гласная)) 0 (repeat filterGraph)

            where filterGraph :: Чтение [Слово]
                  filterGraph = flip fmap graph $ \listWord -> filter ((<=4) . lengthReading) listWord

                  lamCatCond = filterLevelVowel (==)
                  lamCombCond num word =
                    let curVowel = toEnum num :: Гласная
                    in elemLetter (Гласная curVowel) word && filterLevelVowel (<=) num word

                  lam :: (Int -> Слово -> Bool) -> Int -> Чтение [Слово] -> String
                  lam lamCond num curGraph =
                    let curVowel = toEnum num :: Гласная

                    in "## Группа слов с " ++ quote (show curVowel) ++ "\n\n"
                    ++ graphInList (fmap (filter (lamCond num)) curGraph)

                  lamCat = lam lamCatCond
                  lamComb = lam lamCombCond

        lamCycle (templ, Слово word) =
            let transWord = init $ flip deTypingWord word $ \s -> s ++: '|'
            in putStrLn ("Несоответствие: "++quote (showTemplate templ)++" "++quote transWord)

    when (not $ null eqGraph) $ do
        for_ eqGraph lamCycle
        error ""
        
    flip writeFile handlerGraph $ logWriting modeWriting

    where funcGroupArgs [] = ArgsVoid
          funcGroupArgs (a:b:c) = ArgsGroup (read a) b (funcGroupArgs c)
          funcGroupArgs (_:_) = error "Некорректный режим Groups"

          funcReadArgs [x] = ArgsReading x
          funcReadArgs _ = error "Некорректный режим Reading"

{-  | Тест:
    | => readerOne = чтение *: группа' *: подгруппа' *: ударгруппа' ["слово","воск","номер"]
    | => readerTwo = fmap (fmap (fmap (fromJust . charInType))) readerOne
    | => readerThree = fmap (fmap (Слово . hareSystem)) readerTwo
    | => readerFour = fmap (filter ((==4) . lengthReading)) readerThree
    | => readerFour
    | Чтение {группы = [Группа' {подгруппы = [Подгруппа' {ударгруппы = [Ударная'Группа' {слова = [Слово [Двойной (Согласная В) (Гласная О),Одинарный (Согласная С),Одинарный (Согласная К)]]}]}]}]}
-}
