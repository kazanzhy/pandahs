module Demo where

----------------------------------------------------------------------------------------------------------------
newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}
newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

instance Functor (Arr2 e1 e2) where
  --fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  fmap g (Arr2 func) = Arr2 (\x y -> g $ func x y)

instance Functor (Arr3 e1 e2 e3) where
  --fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  fmap g (Arr3 func) = Arr3 (\x y z -> g $ func x y z)

----------------------------------------------------------------------------------------------------------------
data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    --fmap :: (a -> b) -> Triple a -> Triple a
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    --pure :: a -> Triple a
    pure x = (Tr x x x)
    (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)
----------------------------------------------------------------------------------------------------------------

