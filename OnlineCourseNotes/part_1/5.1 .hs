module Demo where

----------------------------------------------------------------------------------------------------------------
instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)
----------------------------------------------------------------------------------------------------------------
instance Functor GeomPrimitive where
    fmap f (Point x) = Point (fmap f x)
    fmap f (LineSegment x y)= LineSegment (fmap f x) (fmap f y)
----------------------------------------------------------------------------------------------------------------
instance Functor Tree where
    fmap g (Leaf (Nothing)) = Leaf Nothing
    fmap g (Leaf (Just x)) = Leaf (Just (g x))
    fmap g (Branch l (Nothing) r) = Branch (fmap g l) (Nothing) (fmap g r)
    fmap g (Branch l (Just x) r) = Branch (fmap g l) (Just (g x)) (fmap g r)
----------------------------------------------------------------------------------------------------------------
import Data.Char

instance Functor (Entry k1 k2) where
    fmap g (Entry (k1, k2) v) = Entry (k1, k2) (g v)

instance Functor (Map k1 k2) where
    fmap g (Map lst) = Map (map (fmap g) lst)
----------------------------------------------------------------------------------------------------------------

