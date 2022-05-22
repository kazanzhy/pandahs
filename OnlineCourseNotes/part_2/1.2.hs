module Demo where

----------------------------------------------------------------------------------------------------------------
import Control.Applicative (ZipList(ZipList), getZipList)

infixl 4 >$<
infixl 4 >*<

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = map

(>*<) :: [a -> b] -> [a] -> [b]
llist >*< rlist = getZipList $ ZipList llist <*> ZipList rlist
----------------------------------------------------------------------------------------------------------------
divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0",1)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> (divideList' xs)
----------------------------------------------------------------------------------------------------------------
newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 arr) = Arr2 $ \e1 e2 -> f (arr e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 arr) = Arr3 $ \e1 e2 e3 -> f (arr e1 e2 e3)

instance Applicative (Arr2 e1 e2) where
  pure x = Arr2 $ \e1 e2 -> x
  (Arr2 larr) <*> (Arr2 rarr) = Arr2 $ \e1 e2 -> (larr e1 e2) (rarr e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
  pure x = Arr3 $ \e1 e2 e3 -> x
  (Arr3 larr) <*> (Arr3 rarr) = Arr3 $ \e1 e2 e3 -> (larr e1 e2 e3) (rarr e1 e2 e3)
----------------------------------------------------------------------------------------------------------------
{-# LANGUAGE RankNTypes#-}
import Control.Applicative ((<**>),ZipList(..))

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Just [1,2] <??> Just head -- place for counterexample

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in [1,2] <??> [(+3),(+0)] -- place for counterexample

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- place for counterexample

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Left "10" <??> Left "20"  -- place for counterexample

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op = 
  let (<??>) = op 
      infixl 4 <??> 
  in ("AA", 3) <??> ("BB",(+1))  -- place for counterexample

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op = 
  let (<??>) = op 
      infixl 4 <??> 
  in length <??> (\_ -> (+5))  -- place for counterexample
----------------------------------------------------------------------------------------------------------------

