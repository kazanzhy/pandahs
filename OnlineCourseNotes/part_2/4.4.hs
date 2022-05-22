module Demo where

{-# LANGUAGE FunctionalDependencies,FlexibleInstances,InstanceSigs #-}

class Functor' c e | c -> e where
    fmap' :: (e -> e) -> c -> c

instance Functor' (Maybe a) a where
    fmap' :: (a -> a) -> Maybe a -> Maybe a
    fmap' f (Just x) = Just $ f x
    fmap' _ Nothing = Nothing

instance Functor' [a] a where
    fmap' :: (a -> a) -> [a] -> [a]
    fmap' = map
    
----------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies,FlexibleInstances,InstanceSigs #-}
import Data.Functor.Identity
import Control.Monad.State

instance MonadState s m => MonadState s (LoggT m) where
    get   = lift get
    put   = lift . put
    state = lift . state

----------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies,FlexibleInstances,InstanceSigs #-}

import Data.Functor.Identity
import Control.Monad.Reader

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = undefined

instance MonadReader r m => MonadReader r (LoggT m) where
    ask    = lift ask
    local f m = LoggT $ local f $ runLoggT m 
    reader = lift . reader 

----------------------------------------------------------------------------------------------------------------
{-# LANGUAGE FunctionalDependencies,FlexibleInstances,InstanceSigs #-}

import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Reader

class Monad m => MonadLogg m where
    w2log :: String -> m ()
    logg :: Logged a -> m a

instance Monad m => MonadLogg (LoggT m) where
    w2log s = LoggT $ return (Logged s ())
    logg x = LoggT $ return x

instance MonadLogg m => MonadLogg (StateT s m) where
    w2log = lift . w2log
    logg  = lift . logg

instance MonadLogg m => MonadLogg (ReaderT r m) where
    w2log = lift . w2log
    logg  = lift . logg
    

