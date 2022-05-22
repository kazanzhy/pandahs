module Demo where

----------------------------------------------------------------------------------------------------------------
import Text.Parsec

getList :: Parsec String u [String]
getList = (many1 digit) `sepBy` (char ';')
----------------------------------------------------------------------------------------------------------------

import Text.Parsec

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces p1 p2 p3 = p1 *> p3 <* p2
----------------------------------------------------------------------------------------------------------------

