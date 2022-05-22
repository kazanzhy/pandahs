module Demo where

----------------------------------------------------------------------------------------------------------------
seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | otherwise = 
            let 
                helper step pprev prev curr = if step == 0 
                                              then curr + prev - 2 * pprev 
                                              else helper (step - 1) prev curr (curr + prev - 2 * pprev)
            in helper (n - 3) 1 2 3

----------------------------------------------------------------------------------------------------------------
import Data.Char
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (toInteger s, toInteger c)
    where
        lst = show $ abs x
        c = length lst
        s = sum (map digitToInt lst)
----------------------------------------------------------------------------------------------------------------
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a == b = 0
                  | otherwise = h * (first + summ)
    where
        h = (b - a) / 1000
        first = (f a + f b) / 2
        summ = sum (map f (init (tail [a, a + h .. b])))
----------------------------------------------------------------------------------------------------------------

