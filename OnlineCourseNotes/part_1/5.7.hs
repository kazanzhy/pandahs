module Demo where

----------------------------------------------------------------------------------------------------------------
evalWriter :: Writer w a -> a
evalWriter = fst . runWriter
----------------------------------------------------------------------------------------------------------------
purchase :: String -> Integer -> Shopping
purchase item cost = tell (Sum cost)

total :: Shopping -> Integer
total = getSum . snd . runWriter
----------------------------------------------------------------------------------------------------------------
type Shopping = Writer (Sum Integer, [String]) ()

purchase :: String -> Integer -> Shopping
purchase item cost = tell (Sum cost, [item])

total :: Shopping -> Integer
total = getSum . fst . snd . runWriter

items :: Shopping -> [String]
items = snd . snd . runWriter
----------------------------------------------------------------------------------------------------------------

