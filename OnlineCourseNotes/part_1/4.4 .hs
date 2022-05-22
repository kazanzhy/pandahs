module Demo where

----------------------------------------------------------------------------------------------------------------
data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)
----------------------------------------------------------------------------------------------------------------
data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter n (Coord x y) = Coord ((fromIntegral x + 0.5) * n) ((fromIntegral y + 0.5) * n)

getCell :: Double -> Coord Double -> Coord Int
getCell n (Coord x y) = Coord (floor (x / n)) (floor (y / n))
----------------------------------------------------------------------------------------------------------------
import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit s = if res /= [] then Just (head res) else Nothing
                where (_,res) = break isDigit s
----------------------------------------------------------------------------------------------------------------
import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX x = case findDigit x of {Nothing -> 'X'; Just y -> y}
----------------------------------------------------------------------------------------------------------------
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x
----------------------------------------------------------------------------------------------------------------
import Data.Text (splitOn, strip, empty, pack, unpack)
import Data.Char (isDigit)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String --deriving (Show)
data Person = Person { firstName :: String, lastName :: String, age :: Int } --deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson inp | (not . and . map (\l -> length l == 2) $ wrs) || (or . map (== empty) $ ws) = Left ParsingError
                | not . and $ map (flip elem ws . pack) ["firstName","lastName","age"] = Left IncompleteDataError
                | notCorrectAge = Left (IncorrectDataError getAge)
                | otherwise = Right (Person {firstName=getFirst, lastName=getLast, age=read getAge :: Int})
                where lns = map pack . lines $ inp
                      wrs = map (\l -> map strip (splitOn (pack "=") l)) lns
                      ws = concat wrs
                      getFirst = unpack . last . head . filter (\(l:_) -> l == pack "firstName") $ wrs
                      getLast = unpack . last . head . filter (\(l:_) -> l == pack "lastName") $ wrs
                      getAge = unpack . last . head . filter (\(l:_) -> l == pack "age") $ wrs
                      notCorrectAge = not . and . map isDigit $ getAge
----------------------------------------------------------------------------------------------------------------
eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing
----------------------------------------------------------------------------------------------------------------
