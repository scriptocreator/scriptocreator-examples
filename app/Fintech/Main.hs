module Main where

import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)
import Text.Printf


newtype Процент a = Процент a deriving (Show, Eq, Ord, Read)
data Заказ a = Заказ a | Заказ' a a deriving (Show, Eq, Ord, Read)


num % perc = (num / 100) * perc

allow :: Заказ Double -> Процент Double -> Int -> (Double, Double)

allow (Заказ orig) mPerc@(Процент perc) calc =
    let newOrder = Заказ' orig orig
    in allow newOrder mPerc calc

allow (Заказ' orig transOrig) mPerc@(Процент perc) calc
    | calc == 0 = error $ printf
        "curTransTotal %f, newTransOrig %f, alligTotal %f, gap %f" curTransTotal newTransOrig alligTotal gap
    | alligTotal >= orig = (curTransTotal, gap)
    | otherwise =
        let correctX = Заказ' orig newTransOrig
        in allow correctX mPerc (pred calc)

    where curTransTotal = transOrig + (transOrig % perc)
          newTransOrig = transOrig + (orig - alligTotal)
          alligTotal = curTransTotal - (curTransTotal % perc)
          gap = curTransTotal - orig

-- => лево = fst; право = snd
-- => счёт = allow (Заказ 1870) (Процент 2)
-- => счёт
-- (1908.1632653061224,38.16326530612241)
-- => доказательство = (лево счёт / право счёт) * 2
-- => доказательство
-- 100.0000000000001


main :: IO ()
main = do
    arg <- getArgs

    let mAll = case arg of
            (a:b:_) ->
                let (mRA, mRB) = (readMaybe a, readMaybe b) :: (Maybe (Заказ Double), Maybe (Процент Double))
                    rAB = (fromJust mRA, fromJust mRB)

                in if isJust mRA && isJust mRB
                    then return rAB
                    else Nothing

            _ -> Nothing

        (ord, perc) = fromJust mAll
    
    if isJust mAll
        then print $ allow ord perc (-1)
        else return ()
