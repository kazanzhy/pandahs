module Demo where

----------------------------------------------------------------------------------------------------------------
import Control.Monad

instance Monad m => Functor (LoggT m) where
    fmap = liftM

instance Monad m => Applicative (LoggT m) where
    pure = return
    (<*>) =  ap
    
instance Monad m => Monad (LoggT m) where
    return x = LoggT $ return $ Logged "" x
    m >>= k  = LoggT $ do
        ~(Logged str a) <- runLoggT m
        ~(Logged str' a') <- runLoggT (k a)
        return (Logged (str `mappend` str') a')
    fail = LoggT . fail

----------------------------------------------------------------------------------------------------------------
write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg = runIdentity . runLoggT
----------------------------------------------------------------------------------------------------------------
instance MonadTrans LoggT where
   -- lift :: LoggT m => m a -> LoggT m a
    lift m = LoggT $ do
        x <- m
        return (Logged "" x)
----------------------------------------------------------------------------------------------------------------

