module Demo where

----------------------------------------------------------------------------------------------------------------
import Control.Monad.Trans.Reader 
import Control.Monad.Trans.Writer
import Control.Monad.Trans (lift)
import Data.Char (toUpper)

logFirstAndRetSecond :: WriterT String (Reader [String]) String
logFirstAndRetSecond = do
    el1 <- lift $ asks head
    el2 <- lift $ asks (map toUpper . head . tail)
    tell el1
    return el2

----------------------------------------------------------------------------------------------------------------
separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 lst = do
    x <- return (filter p1 lst)
    tell x
    y <- return (filter p2 lst)
    lift $ tell y
    z <- return (filter (\x -> not $ p1 x || p2 x) lst)
    return z
----------------------------------------------------------------------------------------------------------------
myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell
----------------------------------------------------------------------------------------------------------------
type MyRWT m = ReaderT [String] (WriterT String m)

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt e = runWriterT (runReaderT rwt e)

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks = asks

myTell :: Monad m => String -> MyRWT m ()
myTell = lift . tell

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift
----------------------------------------------------------------------------------------------------------------
veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
    xs <- myAsk
    let odds = filter (odd . length) xs
    let evens = filter (even . length) xs
    case zip odds evens of
        (el1:el2:_) -> do
            myTell (snd el1)
            myTell (",")
            myTell (fst el1)
            return (map toUpper . snd $ el2, map toUpper . fst $ el2)
        _ -> myLift Nothing
----------------------------------------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

tickCollatz :: State Integer Integer
tickCollatz = do
  n <- get
  let res = if odd n then 3 * n + 1 else n `div` 2
  put res
  return n

type EsSi = ExceptT String (State Integer)

runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi m x = runState (runExceptT m) x

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go l u tick = do
    lift tick
    x <- lift get
    if x >= u then throwE "Upper bound" 
    else if x <= l then throwE "Lower bound"
    else return ()

----------------------------------------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

type RiiEsSiT m = ReaderT (Integer,Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                 -> (Integer,Integer) 
                 -> Integer 
                 -> m (Either String a, Integer)
runRiiEsSiT m r s = runStateT (runExceptT $ runReaderT m r) s

go :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go tick = do
    (l,u) <- ask
    lift (lift tick)
    x <- lift (lift get)
    when (x >= u) (lift $ throwE "Upper bound")
    when (x <= l) (lift $ throwE "Lower bound")

----------------------------------------------------------------------------------------------------------------
