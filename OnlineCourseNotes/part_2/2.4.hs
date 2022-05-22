module Demo where

----------------------------------------------------------------------------------------------------------------
instance Monad PrsE where
  (>>=) (PrsE prs) k = PrsE func where
      func str = do
          (x, xs) <- prs str
          (y, ys) <- runPrsE (k x) xs
          return (y, ys)
----------------------------------------------------------------------------------------------------------------
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x y z) sc th = Bi x y (concat3OC z sc th)
concat3OC (Un v) (Bi x y z) th = Bi v x (concat3OC (Un y) z th)
concat3OC (Un t) (Un u) th = Bi t u th
----------------------------------------------------------------------------------------------------------------
concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x y z) sc th = Bi x y (concat3OC z sc th)
concat3OC (Un v) (Bi x y z) th = Bi v x (concat3OC (Un y) z th)
concat3OC (Un t) (Un u) th = Bi t u th

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un v) = v
concatOC (Bi x y z) = concat3OC x y (concatOC z)
----------------------------------------------------------------------------------------------------------------
instance Functor OddC where
    fmap f (Un x) = Un (f x)
    fmap f (Bi x y od) = Bi (f x) (f y) (fmap f od)

instance Applicative OddC where
    pure x = Un x
    (Bi f g h) <*> con@(Bi x y z) = concat3OC (f <$> con) (g <$> con) (h <*> con)
    (Bi f g h) <*> con@(Un x) = concat3OC (f <$> con) (g <$> con) (h <*> con)
    (Un f) <*> (Bi x y od) = Bi (f x) (f y) (f <$> od)
    (Un f) <*> (Un x) = Un (f x)
    
instance Foldable OddC where
    foldr f ini (Un x) = f x ini
    foldr f ini (Bi x y od) = f x (f y (foldr f ini od))
      
instance Traversable OddC where
    traverse g (Un x) = pure Un <*> g x
    traverse g (Bi x y od) = pure Bi <*> g x <*> g y <*> (traverse g od)
     
instance Monad OddC where
    (Un x) >>= k = k x
    (Bi x y od) >>= k = concatOC $ Bi (k x) (k y) (Un $ od >>= k)
    

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x y z) sc th = Bi x y (concat3OC z sc th)
concat3OC (Un v) (Bi x y z) th = Bi v x (concat3OC (Un y) z th)
concat3OC (Un t) (Un u) th = Bi t u th

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un v) = v
concatOC (Bi x y z) = concat3OC x y (concatOC z)
----------------------------------------------------------------------------------------------------------------

