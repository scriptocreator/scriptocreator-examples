module Main where

import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)


newtype Процент a = Процент a deriving (Show, Eq, Ord, Read)
newtype Заказ a = Заказ a deriving (Show, Eq, Ord, Read)


allowance :: Процент Double -> Заказ Double -> (Double, Double)
allowance (Процент percent) (Заказ order)
    = (order + sum, sum)

    where sum = order / 100 * percent

-- => лево = fst; право = snd
-- => счёт = allowance (Процент 2) (Заказ 1870)
-- => счёт
-- (1907.4,37.4)
-- => лево счёт - право счёт
-- 1907.4


main :: IO ()
main = do
    arg <- getArgs

    let mAll = case arg of
            (a:b:_) ->
                let (mRA, mRB) = (readMaybe a, readMaybe b) :: (Maybe (Процент Double), Maybe (Заказ Double))
                    rAB = (fromJust mRA, fromJust mRB)

                in if isJust mRA && isJust mRB
                    then return rAB
                    else Nothing

            _ -> Nothing

        (perc, ord) = fromJust mAll
    
    if isJust mAll
        then print $ allowance perc ord
        else return ()
