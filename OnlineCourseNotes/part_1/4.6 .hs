module Demo where

----------------------------------------------------------------------------------------------------------------
newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
    mempty = Xor False
    mappend a b = Xor (not $ getXor a == getXor b)
----------------------------------------------------------------------------------------------------------------
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just (mempty))
    
    Maybe' Nothing `mappend` mempty = Maybe' Nothing
    mempty `mappend` Maybe' Nothing = Maybe' Nothing
    
    --x `mappend` mempty = x
    --mempty `mappend` x = x
    
    Maybe' x `mappend` Maybe' y = Maybe' (x `mappend` y)
----------------------------------------------------------------------------------------------------------------
import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where
    empty = ListMap []

    lookup x (ListMap []) = Nothing
    lookup x (ListMap ((k,v):xs)) | x == k = Just v 
                                  | otherwise = lookup x (ListMap xs)
                     
    insert x y (ListMap []) = ListMap [(x,y)]
    insert x y (ListMap ((k,v):xs)) | x == k = ListMap ((k,y):xs)
                                    | otherwise = ListMap ((k,v) : (getListMap $ insert x y (ListMap xs)))
                                     
    delete x (ListMap []) = ListMap []
    delete x (ListMap ((k,v):xs)) | x == k = ListMap xs 
                                  | otherwise = ListMap ((k,v) : (getListMap $ delete x (ListMap xs)))
----------------------------------------------------------------------------------------------------------------
import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (\x -> Nothing)

    lookup k (ArrowMap f) =  f k 
   
    insert k v (ArrowMap f) = ArrowMap (\x -> if x == k then Just v else f x)

    delete k (ArrowMap f) = ArrowMap (\x -> if x == k then Nothing else f x)
     
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)
----------------------------------------------------------------------------------------------------------------

