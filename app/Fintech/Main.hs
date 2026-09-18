module Main where

import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)
import Text.Printf
import Data.Word


newtype Процент a = Процент a deriving (Show, Eq, Ord, Read)
data Заказ a = Заказ a | Заказ' a a deriving (Show, Eq, Ord, Read)


data Mode a
    = Run
    | Stop
    | Debug !a
    deriving (Show, Eq, Ord, Read)

class NumFunctor f where
  nmap :: (Num a, Eq a) => (a -> a) -> f a -> f a

instance NumFunctor Mode where
    nmap f (Debug 0) = Stop
    nmap f (Debug calc) = Debug (f calc)
    nmap _ Stop = Stop
    nmap _ Run = Run


data Step = Step
    { curTransTotal :: !Double
    , newTransOrig  :: !Double
    , alligTotal    :: !Double
    , gap           :: !Double
    }

compute :: Заказ Double -> Процент Double -> Step
compute (Заказ' orig transOrig) (Процент perc)
    = Step curTransTotal newTransOrig alligTotal gap
    
    where curTransTotal = transOrig + (transOrig % perc)
          newTransOrig  = transOrig + (orig - alligTotal)
          alligTotal    = curTransTotal - (curTransTotal % perc)
          gap           = curTransTotal - orig


(%) :: Double -> Double -> Double
num % perc = (num / 100) * perc

infixl 1 $.
($.) :: (a -> b) -> a -> b
f $. x = f x


allow :: Заказ Double -> Процент Double -> Mode Word -> (Double, Double)

allow (Заказ orig) perc calc = allow (Заказ' orig orig) perc calc

allow order perc Stop = error $ printf
    $. "curTransTotal = %f, newTransOrig = %f, alligTotal = %f, gap = %f"
    $. curTransTotal s
    $. newTransOrig s
    $. alligTotal s
    $. gap s

    where s = compute order perc

allow order@(Заказ' orig _) perc calc
    | alligTotal s >= orig = (curTransTotal s, gap s)
    | otherwise = allow correctX perc newCalc

    where s = compute order perc
          correctX = Заказ' orig (newTransOrig s)
          newCalc = nmap pred calc

-- => лево = fst; право = snd
-- => счёт = allow (Заказ 1870) (Процент 2)
-- => счёт
-- (1908.1632653061224,38.16326530612241)
-- => доказательство = (лево счёт / право счёт) * 2
-- => доказательство
-- 100.0000000000001


main :: IO ()
main = do
    args <- getArgs
    case args of
        (a:b:_) ->
            let mOrdPerc :: (Maybe (Заказ Double), Maybe (Процент Double))
                mOrdPerc = (readMaybe a, readMaybe b)

            in case mOrdPerc of
                (Just ord, Just perc) -> print (allow ord perc Run)
                _                     -> pure ()

        _ -> pure ()
