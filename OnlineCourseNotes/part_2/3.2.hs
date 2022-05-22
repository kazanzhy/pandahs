module Demo where

----------------------------------------------------------------------------------------------------------------
decode c = c 0
as x c = c x
a x c = c x
number = id

one x c = c (x+1)
two x c = c (x+2)
three x c = c (x+3)
seventeen x c = c (x+17)
twenty x c = c (x+20)
hundred x c = c (x*100)
thousand x c = c (x*1000)
----------------------------------------------------------------------------------------------------------------
showCont :: Show a => Cont String a -> String
showCont m = runCont m show
----------------------------------------------------------------------------------------------------------------
type Checkpointed a = (a -> Cont a a) -> Cont a a

runCheckpointed :: (a -> Bool) -> Checkpointed a -> a
runCheckpointed f exp = runCont (exp checkpoint) id where
    checkpoint x = do 
        Cont $ \c -> if (f $ c x) then c x else x
----------------------------------------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad

newtype FailCont r e a = FailCont {runFailCont :: (a -> r) -> (e -> r) -> r }

instance Functor (FailCont r e) where
    fmap = liftM
    
instance Applicative (FailCont r e) where
    pure = return
    (<*>) = ap  
    
instance Monad (FailCont r e) where
    --return :: a -> FailCont r e a
    return x = FailCont $ \a _ -> a x
    --(>>=) :: FailCont r e a -> (a -> FailCont r e b) -> FailCont r e b
    FailCont v >>= k = FailCont $ 
        \c d -> v (\a -> runFailCont (k a) c d) d
        
toFailCont :: Except e a -> FailCont r e a
toFailCont exc = FailCont $ case runExcept exc of
    Right a -> \ok _ -> ok a
    Left e -> \_ err -> err e
    
evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont m = runFailCont m Right Left
----------------------------------------------------------------------------------------------------------------
callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a
callCFC f = FailCont $ \c cc -> runFailCont (f $ \a -> FailCont $ \_ _ -> c a) c cc
----------------------------------------------------------------------------------------------------------------

