module Demo where

----------------------------------------------------------------------------------------------------------------
withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept f exc = case runExcept exc of
    Right x -> Except (Right x)
    Left s -> Except (Left $ f s)
----------------------------------------------------------------------------------------------------------------
import Control.Monad.Trans.Except

infixl 9 !!!

(!!!) :: [a] -> Int -> Except ListIndexError a
lst !!! i | i < 0 = throwE $ ErrNegativeIndex
          | i >= length (take (i+1) lst) = throwE $ ErrIndexTooLarge i
          | otherwise = return $ lst !! i
----------------------------------------------------------------------------------------------------------------
tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE $ EmptyInput
tryRead s = case reads s of
    [] -> throwE $ NoParse s
    x:xs -> if (snd x) == "" 
        then return $ fst x 
        else throwE $ NoParse s
----------------------------------------------------------------------------------------------------------------
trySum :: [String] -> Except SumError Integer
trySum = fmap sum . sequence . zipWith (withExcept . SumError) [1..] . fmap tryRead
----------------------------------------------------------------------------------------------------------------
instance Monoid SimpleError where
    mempty = Simple ""
    Simple x `mappend` Simple y = Simple $ x ++ y

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"
----------------------------------------------------------------------------------------------------------------
instance Functor (Validate e) where
    fmap f = Validate . fmap f . getValidate
  
instance Applicative (Validate e) where
    pure = Validate . pure
    Validate (Right f) <*> vx = f <$> vx
    vf <*> Validate (Right x) = ($ x) <$> vf
    Validate (Left es1) <*> Validate (Left es2) = Validate $ Left (es1 `mappend` es2)
    
collectE :: Except e a -> Validate e a
collectE x = case runExcept x of
    Right r -> Validate (Right r)
    Left l -> Validate (Left [l])

validateSum :: [String] -> Validate SumError Integer
validateSum = fmap sum . traverse collectE . zipWith (withExcept . SumError) [1..] . fmap tryRead
----------------------------------------------------------------------------------------------------------------
