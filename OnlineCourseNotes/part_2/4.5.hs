module Demo where

----------------------------------------------------------------------------------------------------------------
import Control.Monad.Except

tryRead :: (Read a, MonadError ReadError m) => String -> m a
tryRead str
  | null str = throwError EmptyInput
  | otherwise = case reads str of
      [(n,"")] -> return n
      _        -> throwError $ NoParse str

----------------------------------------------------------------------------------------------------------------
treeSum :: Tree String -> Either ReadError Integer
treeSum t = err >>= (\_ -> Right $ getSum s) where
    (err, s) = runWriter . runExceptT $ traverse_ go t

go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go tr = do
    x <- tryRead tr
    lift $ tell (Sum x)
    
----------------------------------------------------------------------------------------------------------------
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Data.Foldable


run1 :: ExceptT Int (State s) [a] -> s -> (Either Int [a], s)
run1 m s = runState (runExceptT m) s

run2 :: StateT s (Except Int) [a] -> s -> Either Int ([a], s)
run2 m s = runIdentity $ runExceptT $ runStateT m s

----------------------------------------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Either (isRight)

newtype CoroutineT m a = CoroutineT { runCoroutineT :: m (Either (CoroutineT m a) a) }

instance Monad m => Functor (CoroutineT m) where
    fmap = liftM
    
instance Monad m => Applicative (CoroutineT m) where
    pure = return
    (<*>) = ap  
    
instance Monad m => Monad (CoroutineT m) where
    return = CoroutineT . return . Right
    CoroutineT m >>= f = CoroutineT $ do
        v <- m
        case v of
            Right  a -> runCoroutineT (f a)
            Left b -> return (Left (b >>= f))

instance MonadTrans CoroutineT where
    lift m = CoroutineT (liftM Right m)

instance MonadWriter w m => MonadWriter w (CoroutineT m) where
    tell = lift . tell
    listen = undefined
    pass = undefined


yield :: Monad m => CoroutineT m ()
yield = CoroutineT . return . Left $ return ()


runCoroutines :: (Monad m) => CoroutineT m () -> CoroutineT m () -> m ()
runCoroutines xc yc = do
    x <- runCoroutineT xc
    y <- runCoroutineT yc
    if (isRight x && isRight y) then
        return ()
    else if (isRight x) then 
        runCoroutines (unLeft y) (return ())
    else if (isRight y) then 
        runCoroutines (return ()) (unLeft x)
    else
        runCoroutines (unLeft x) (unLeft y)
    where
    unLeft (Left l) = l
