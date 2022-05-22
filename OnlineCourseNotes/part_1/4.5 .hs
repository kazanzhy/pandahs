module Demo where

----------------------------------------------------------------------------------------------------------------
data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = a : (fromList b)

toList :: [a] -> List a
toList [] = Nil
toList (x : xs) = Cons x (toList xs)
----------------------------------------------------------------------------------------------------------------
data Nat = Zero | Suc Nat

fact :: Integer -> Integer
fact 0 = 1
fact x = x * (fact $ x - 1)

toNat :: Integer -> Nat
toNat 0 = Zero
toNat x = Suc (toNat (x - 1))

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add x y = toNat ((fromNat x) + (fromNat y))

mul :: Nat -> Nat -> Nat
mul x y = toNat ((fromNat x) * (fromNat y))

fac :: Nat -> Nat
fac x = toNat (fact $ fromNat x)
----------------------------------------------------------------------------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf x) = 0
height (Node x y) = ((height x) `max` (height y)) + 1

size :: Tree a -> Int
size (Leaf x) = 1
size (Node x y) = (size x) + (size y) + 1
----------------------------------------------------------------------------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1,x)
    go (Node x y) = (fst (go x) + fst (go y), snd (go x) + snd (go y))
----------------------------------------------------------------------------------------------------------------
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)
    
expand :: Expr -> Expr
expand e = if e == expanded then e else expand expanded
            where expanded = expand' e

expand' :: Expr -> Expr
expand' ((e1 :+: e2) :*: e) = expand' e1 :*: expand' e :+: expand' e2 :*: expand' e
expand' (e :*: (e1 :+: e2)) = expand' e :*: expand' e1 :+: expand' e :*: expand' e2
expand' (e1 :+: e2) = expand' e1 :+: expand' e2
expand' (e1 :*: e2) = expand' e1 :*: expand' e2
expand' e = e
----------------------------------------------------------------------------------------------------------------

