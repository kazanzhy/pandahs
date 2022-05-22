module Demo where

----------------------------------------------------------------------------------------------------------------
arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T (\x y -> return $ f x y)

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T (\x y z -> return $ f x y z)

----------------------------------------------------------------------------------------------------------------
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

instance Monad m => Functor (Arr2T e1 e2 m) where
    --fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
    fmap f rdr = Arr2T $ \x y -> fmap f $ getArr2T rdr x y

instance Monad m => Functor (Arr3T e1 e2 e3 m) where
    --fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
    fmap f rdr = Arr3T $ \x y z -> fmap f $ getArr3T rdr x y z
----------------------------------------------------------------------------------------------------------------
newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

instance Monad m => Functor (Arr2T e1 e2 m) where
    --fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
    fmap f rdr = Arr2T $ \x y -> f <$> (getArr2T rdr) x y

instance Monad m => Functor (Arr3T e1 e2 e3 m) where
    --fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
    fmap f rdr = Arr3T $ \x y z -> f <$> (getArr3T rdr) x y z
    
instance (Monad m, Applicative m) => Applicative (Arr2T e1 e2 m) where
    --pure :: a -> Arr2T e1 e2 m a
    pure x = Arr2T $ \_ _ -> pure x
    --(<*>) :: Arr2T e1 e2 m (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
    f <*> v = Arr2T $ \e1 e2 -> getArr2T f e1 e2 <*> getArr2T v e1 e2
    
instance (Monad m, Applicative m) => Applicative (Arr3T e1 e2 e3 m) where
    --pure :: a -> Arr3T e1 e2 e3 m a
    pure x = Arr3T $ \_ _ _ -> pure x
    --(<*>) :: Arr3T e1 e2 e3 m (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
    f <*> v = Arr3T $ \e1 e2 e3 -> getArr3T f e1 e2 e3 <*> getArr3T v e1 e2 e3
----------------------------------------------------------------------------------------------------------------
import Control.Monad

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

instance (Functor m, Monad m) => Functor (Arr2T e1 e2 m) where
    fmap = liftM

instance (Functor m, Monad m) => Functor (Arr3T e1 e2 e3 m) where
    fmap = liftM
    
instance (Applicative m, Monad m) => Applicative (Arr2T e1 e2 m) where
    pure = return
    (<*>) =  ap
   
instance (Applicative m, Monad m) => Applicative (Arr3T e1 e2 e3 m) where
    pure = return
    (<*>) =  ap

instance (Monad m) => Monad (Arr2T e1 e2 m) where
    --return :: a -> Arr2T e1 e2 m a
    return x = Arr2T $ \_ _ -> pure x
    --(>>=) :: Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
    m >>= k = Arr2T $ \e1 e2 -> do
        v <- getArr2T m e1 e2
        getArr2T (k v) e1 e2
    
instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
    --return :: a -> Arr3T e1 e2 e3 m a
    return x = Arr3T $ \_ _ _ -> pure x
    --(>>=) :: Arr3T e1 e2 e3 m a -> (a -> Arr3T e1 e2 e3 m b) -> Arr3T e1 e2 e3 m b
    m >>= k = Arr3T $ \e1 e2 e3 -> do
        v <- getArr3T m e1 e2 e3
        getArr3T (k v) e1 e2 e3
----------------------------------------------------------------------------------------------------------------
import Control.Monad

newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

instance (Functor m, Monad m) => Functor (Arr3T e1 e2 e3 m) where
    fmap = liftM
   
instance (Applicative m, Monad m) => Applicative (Arr3T e1 e2 e3 m) where
    pure = return
    (<*>) =  ap

instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where
    fail x = Arr3T $ \_ _ _ -> fail x
    return x = Arr3T $ \_ _ _ -> pure x
    m >>= k = Arr3T $ \e1 e2 e3 -> do
        v <- getArr3T m e1 e2 e3
        getArr3T (k v) e1 e2 e3
----------------------------------------------------------------------------------------------------------------
import Control.Monad

class MonadTrans t where
  lift :: Monad m => m a -> t m a

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }

instance (Functor m, Monad m) => Functor (Arr2T e1 e2 m) where
    fmap = liftM
   
instance (Applicative m, Monad m) => Applicative (Arr2T e1 e2 m) where
    pure = return
    (<*>) =  ap
    
instance (Monad m) => Monad (Arr2T e1 e2 m) where
    --return :: a -> Arr2T e1 e2 m a
    return x = Arr2T $ \_ _ -> pure x
    --(>>=) :: Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
    m >>= k = Arr2T $ \e1 e2 -> do
        v <- getArr2T m e1 e2
        getArr2T (k v) e1 e2
        
instance MonadTrans (Arr2T e1 e2) where
    --lift :: Monad m => m a -> Arr2T e1 e2 m a
    lift m = Arr2T $ \_ _ -> m
    
--asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return (f e1 e2)
