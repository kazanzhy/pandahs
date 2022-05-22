module Demo where

----------------------------------------------------------------------------------------------------------------
traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\x y -> pure (:) <*> f x <*> y) (pure [])
----------------------------------------------------------------------------------------------------------------
instance Traversable Triple where
    traverse g (Tr x y z) = pure Tr <*> g x <*> g y <*> g z
----------------------------------------------------------------------------------------------------------------
instance Functor Result where
    fmap f (Ok x) = Ok (f x)
    fmap f (Error s) = Error s

instance Applicative Result where
    pure x = Ok x
    (Ok f) <*> (Ok x) = Ok (f x)
    (Error s) <*> (Ok x) = Error s
    (Ok f) <*> (Error s) = Error s
    (Error s1) <*> (Error s2) = Error (s1 ++ s2)
    
instance Foldable Result where
    foldr f ini (Ok x) = x `f` ini
    foldr f ini (Error s) = ini
      
instance Traversable Result where
    traverse g (Ok x) = pure Ok <*> g x
    traverse _ (Error s) = pure $ Error s
----------------------------------------------------------------------------------------------------------------
instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure x = Branch Nil x Nil
    (Branch fl f fr) <*> (Branch l x r) = Branch (fl <*> l) (f x) (fr <*> r)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    
instance Foldable Tree where 
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = f x $ foldr f (foldr f ini r) l -- Pre order
    --foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l -- In order
      
instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse g (Branch l x r) = pure Branch <*> traverse g l <*> g x <*> traverse g r
---------------------------------------------------------------------------------------------------------------- 
instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
    --traverse :: Applicative f  => (a -> f b) -> t a -> f (t b)
    traverse h (Cmps x) = pure Cmps <*> (traverse (traverse h) x)
    
