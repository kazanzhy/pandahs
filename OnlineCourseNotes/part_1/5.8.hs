module Demo where

----------------------------------------------------------------------------------------------------------------
readerToState :: Reader r a -> State r a
readerToState m = State $ \st -> ((runReader m $ st), st)
----------------------------------------------------------------------------------------------------------------
writerToState :: Monoid w => Writer w a -> State w a
writerToState m = State $ \st -> (a, st `mappend` w) where 
    (a, w) = runWriter m
----------------------------------------------------------------------------------------------------------------
import Control.Monad

fibStep :: State (Integer, Integer) ()
fibStep = State $ \(p,n) -> ((), (n, p + n))

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ (replicateM n m)
----------------------------------------------------------------------------------------------------------------
numberTree :: Tree () -> Tree Integer
numberTree tree = fst $ numTr tree 1

numTr :: Tree () -> Integer -> (Tree Integer, Integer)
numTr (Leaf ()) acc = (Leaf acc, acc + 1)
numTr (Fork l x r) acc = let 
        (left, acc') = numTr l acc
        (right, acc'') = numTr r (acc' + 1)
    in ((Fork left acc' right), acc'')
----------------------------------------------------------------------------------------------------------------

