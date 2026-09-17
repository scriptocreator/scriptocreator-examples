module Main where

import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)


newtype Процент a = Процент a deriving (Show, Eq, Ord, Read)
data Заказ a = Заказ a | Заказ' a a deriving (Show, Eq, Ord, Read)


num % perc = (num / 100) * perc

allow (Заказ orig) mPerc@(Процент perc) =
    let newOrder = Заказ' orig orig
    in allow newOrder mPerc

allow (Заказ' orig transOrig) mPerc@(Процент perc)
    | alligTotal >= orig = (curTransTotal, gap)
    | otherwise =
        let correctX = Заказ' orig newTransOrig
        in allow correctX mPerc

    where curTransTotal = transOrig + (transOrig % perc)
          newTransOrig = transOrig + (orig - alligTotal)
          alligTotal = curTransTotal - (curTransTotal % perc)
          gap = curTransTotal - orig

-- => лево = fst; право = snd
-- => счёт = allow (Заказ 1870) (Процент 2)
-- => счёт
-- (1908.1632653061224,38.16326530612241)
-- => лево счёт - право счёт
-- 1908.1632653061224


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
        then print $ allow ord perc
        else return ()
