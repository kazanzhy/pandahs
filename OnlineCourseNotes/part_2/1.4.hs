module Demo where

----------------------------------------------------------------------------------------------------------------
instance Functor Prs where
  fmap f (Prs prs) = Prs func where
      func str = case prs str of 
          Just (x, y) -> Just (f x, y)
          Nothing -> Nothing

anyChr :: Prs Char
anyChr = Prs func where
   func [] = Nothing
   func (x:xs) = Just (x, xs)
----------------------------------------------------------------------------------------------------------------
instance Applicative Prs where
  pure x = Prs (\str -> Just (x, str))
  (Prs lp) <*> (Prs rp) = Prs fun where
      fun str = do
          (g, s) <- lp str
          (x, y) <- rp s
          return (g x, y)
----------------------------------------------------------------------------------------------------------------
satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE fn = PrsE fun where
    fun "" = Left "unexpected end of input"
    fun (x:xs) | fn x = Right (x, xs)
               | otherwise = Left ("unexpected " ++ [x])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)
----------------------------------------------------------------------------------------------------------------
instance Functor PrsE where
  fmap fn (PrsE prs) = PrsE func where
      func str = do
          (x, xs) <- prs str
          return (fn x, xs)
      
instance Applicative PrsE where
  pure fn = PrsE func where
      func str = Right (fn, str)
  (PrsE lp) <*> (PrsE rp) = PrsE func where
      func str = do 
          (g, s) <- lp str
          (x, xs) <- rp s
          return (g x, xs)
----------------------------------------------------------------------------------------------------------------
instance Alternative Prs where
  empty = Prs func where
      func _ = Nothing
  (Prs lp) <|> (Prs rp) = Prs func where
      func s = case lp s of 
          Nothing -> rp s
          Just _ -> lp s
----------------------------------------------------------------------------------------------------------------
many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

----------------------------------------------------------------------------------------------------------------
import Data.Char

digit :: Prs Char
digit = Prs fun where
    fun "" = Nothing
    fun (x:xs) | isDigit x = Just (x, xs)
               | otherwise = Nothing

strToInt :: [Char] -> Int
strToInt x = read x :: Int

many1 :: Prs a -> Prs [a]
many1 p = (:) <$> p <*> (many1 p <|> pure [])

nat :: Prs Int
nat = strToInt <$> many1 digit
----------------------------------------------------------------------------------------------------------------

