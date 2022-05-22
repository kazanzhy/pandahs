module Demo where

----------------------------------------------------------------------------------------------------------------

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = fmap fst $ runStateT m s

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = fmap snd $ runStateT m s

----------------------------------------------------------------------------------------------------------------
readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT m = StateT $ \e -> do
    x <- runReaderT m e
    return (x, e)
----------------------------------------------------------------------------------------------------------------
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m = fmap snd . runStateT m 

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m = fmap fst . runStateT m

instance Functor m => Functor (StateT s m) where
  fmap f m = StateT $ \st -> fmap updater $ runStateT m st
    where updater ~(x, s) = (f x, s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \ s -> return (x, s)
  f <*> v = StateT $ \ s -> do
      ~(g, s') <- runStateT f s
      ~(x, s'') <- runStateT v s'
      return (g x, s'')

instance Monad m => Monad (StateT s m) where
  fail x = StateT $ \_ -> fail x
  m >>= k  = StateT $ \s -> do
    ~(x, s') <- runStateT m s
    runStateT (k x) s'
----------------------------------------------------------------------------------------------------------------
go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go m = StateT $ \e -> writer (numTr m e, Sum (cntTr m 0)) where
    numTr :: Tree () -> Integer -> (Tree Integer, Integer)
    numTr (Leaf ()) acc = (Leaf acc, acc + 1)
    numTr (Fork l x r) acc = let 
        (left, acc') = numTr l acc
        (right, acc'') = numTr r (acc' + 1)
        in ((Fork left acc' right), acc'')
    cntTr :: Tree () -> Integer -> Integer
    cntTr (Leaf ()) acc = acc + 1
    cntTr (Fork lt x rt) acc = cntTr lt acc + cntTr rt acc
----------------------------------------------------------------------------------------------------------------
