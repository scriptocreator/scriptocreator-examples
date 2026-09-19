module Main where

import System.Environment (getArgs)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Text.Read (readMaybe)
import Text.Printf
import Data.Word


newtype Процент a = Процент a deriving (Show, Eq, Ord, Read)
data Заказ a = Заказ a | Заказ' a a deriving (Show, Eq, Ord, Read)


data Mode
    = Run
    | Stop
    | Debug !Word
    deriving (Show, Eq, Ord, Read)

stepMode :: Mode -> Mode
stepMode (Debug 0) = Stop
stepMode (Debug n) = Debug (pred n)
stepMode Stop      = Stop
stepMode Run       = Run


data Step = Step
    { curTransTotal :: !Double
    , newTransOrig  :: !Double
    , alligTotal    :: !Double
    , gap           :: !Double
    }

compute :: Заказ Double -> Процент Double -> Step
compute order (Процент perc)
    = Step curTransTotal newTransOrig alligTotal gap
    
    where (orig, transOrig) = case order of
            Заказ orig -> (orig, orig)
            Заказ' orig transOrig -> (orig, transOrig)
        
          curTransTotal = transOrig + (transOrig % perc)
          newTransOrig  = transOrig + (orig - alligTotal)
          alligTotal    = curTransTotal - (curTransTotal % perc)
          gap           = curTransTotal - orig


(%) :: Double -> Double -> Double
num % perc = (num / 100) * perc

infixl 1 $.
($.) :: (a -> b) -> a -> b
f $. x = f x


allow :: Заказ Double -> Процент Double -> Mode -> (Double, Double)

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
          newCalc = stepMode calc

-- => лево = fst; право = snd
-- => счёт = allow (Заказ 1870) (Процент 2) Run
-- => счёт
-- (1908.1632653061224,38.16326530612241)
-- => доказательство = (лево счёт / право счёт) * 2
-- => доказательство
-- 100.0000000000001


main :: IO ()
main = do
    args <- getArgs

    let requestHandler :: (String, String, Maybe String) -> IO ()
        requestHandler (a, b, mC) =
            let mOrdPerc :: (Maybe (Заказ Double), Maybe (Процент Double), Maybe (Maybe Mode))
                mOrdPerc = (readMaybe a, readMaybe b, fmap readMaybe mC)

            in case mOrdPerc of
                (Just ord, Just perc, mmM) | fromMaybe True (fmap isJust mmM) ->
                    let mM = fmap fromJust mmM
                    in print $ allow ord perc $ fromMaybe Run mM
                _ -> pure ()

    case args of
        (a:b:c:_) -> requestHandler (a, b, Just c)
        (a:b:_) -> requestHandler (a, b, Nothing)
        _ -> pure ()
