module Demo where

----------------------------------------------------------------------------------------------------------------
satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP fn = PrsEP fun where
    fun i "" = (i+1, Left ("pos " ++ show (i+1) ++ ": unexpected end of input"))
    fun i (x:xs) | fn x = (i+1, Right (x, xs))
                 | otherwise = (i+1, Left ("pos " ++ show (i+1) ++ ": unexpected " ++ [x]))
----------------------------------------------------------------------------------------------------------------
instance Functor PrsEP where
    fmap fn (PrsEP prs) = PrsEP func where
        func i str = case prs i str of
            (i, Left s) -> (i, Left s)
            (i, Right (x, xs)) -> (i, Right (fn x, xs))

instance Applicative PrsEP where
    pure x = PrsEP func where
        func i str = (i, Right (x, str))
    (PrsEP lp) <*> (PrsEP rp) = PrsEP func where
        func i str = case lp i str of
            (i, Left s) -> (i, Left s)
            (i, Right (g, s)) -> case rp i s of
                (i, Right (x, xs)) -> (i, Right (g x, xs))
                (i, Left s) -> (i, Left s)    
----------------------------------------------------------------------------------------------------------------
import Control.Applicative

instance Alternative PrsEP where
    empty = PrsEP func where
        func i str = (i, Left $ "pos " ++ show i ++ ": empty alternative")
    (PrsEP lp) <|> (PrsEP rp) = PrsEP func where
        func i str = case lp i str of
            (li, Right lres) -> (li, Right lres)
            (li, Left lerr) -> case rp i str of
                (ri, Right rres) -> (ri, Right rres)
                (ri, Left rerr) -> if li >= ri then (li, Left lerr) else (ri, Left rerr)
----------------------------------------------------------------------------------------------------------------

