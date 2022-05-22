module Demo where

----------------------------------------------------------------------------------------------------------------
instance Foldable Triple where
    foldr f ini (Tr x y z) = x `f` (y `f` (z `f` ini))
    foldl f ini (Tr x y z) = ini `f` x `f` y `f` z
----------------------------------------------------------------------------------------------------------------
instance Foldable Tree where -- In order
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

instance Foldable Preorder where
    foldr f ini (PreO tr) = foldr' f ini tr where
        foldr' f ini Nil = ini
        foldr' f ini (Branch l x r) = f x (foldr' f (foldr' f ini r) l)

instance Foldable Postorder where
    foldr f ini (PostO tr) = foldr' f ini tr where
        foldr' f ini Nil = ini
        foldr' f ini (Branch l x r) = foldr' f (foldr' f (f x ini) r) l

instance Foldable Levelorder where
    foldr f ini (LevelO tr) = foldr' f ini (concat $ levels tr) where
        foldr' f ini [] = ini
        foldr' f ini (x:xs) = f x (foldr' f ini xs)
   
       
merge :: [[b]] -> [[b]] -> [[b]]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = (x ++ y) : (merge xs ys)       
            
levels :: Tree a -> [[a]]
levels Nil = [[]]
levels (Branch l x r) = [[x]] ++ (merge (levels l) (levels r))
----------------------------------------------------------------------------------------------------------------
--newtype Endo a = Endo {appEndo :: a -> a}
import Data.Monoid (Endo(..))

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo cont = Endo (foldr (.) id cont)
----------------------------------------------------------------------------------------------------------------
instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
    --foldMap :: (a -> m) -> t a -> m
    foldMap h (Cmps x) = foldMap (foldMap h) x
----------------------------------------------------------------------------------------------------------------
