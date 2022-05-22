module Demo where

----------------------------------------------------------------------------------------------------------------
import Data.Char
-- data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
--     deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

asToken :: String -> Maybe Token
asToken x | all isDigit x = Just (Number (read x :: Int))
          | x == "+" = Just Plus
          | x == "-" = Just Minus
          | x == "(" = Just LeftBrace
          | x == ")" = Just RightBrace
          | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . map (asToken) . words
----------------------------------------------------------------------------------------------------------------
nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred | n < 0 = []
                        | n == 0 = if pred b then [b] else []
                        | otherwise = do 
                                    p <- nextPositions b
                                    nextPositionsN p (n-1) pred
----------------------------------------------------------------------------------------------------------------
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do 
    c <- [1..x]
    b <- [1..c]
    a <- [1..b]
    if c^2 == a^2 + b^2 then [1] else []
    return (a,b,c)
----------------------------------------------------------------------------------------------------------------

