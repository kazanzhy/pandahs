module Demo where

----------------------------------------------------------------------------------------------------------------
instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"
----------------------------------------------------------------------------------------------------------------
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
----------------------------------------------------------------------------------------------------------------
data Color = Red | Green | Blue deriving Show

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue
----------------------------------------------------------------------------------------------------------------
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info Info = EQ

cmp Error _ = GT
cmp _ Error = LT

cmp _ Info = GT
cmp Info _ = LT
----------------------------------------------------------------------------------------------------------------
processData :: SomeData -> String
processData x = case doSomeWork x of (Success,_) -> "Success"
                                     (Fail,x)    -> "Fail: " ++ show x
----------------------------------------------------------------------------------------------------------------
