module Demo where

----------------------------------------------------------------------------------------------------------------
moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves gm 0 init = do
    case gm init of 
        Floor -> return (Right init)
        Chasm -> return (Left Fallen)
        Snake -> return (Left Poisoned)
moves gm steps init@(x, y) = do
    case gm init of
        Chasm -> return (Left Fallen)
        Snake -> return (Left Poisoned)
        Floor -> do
            w <- [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
            moves gm (steps-1) w

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie dr gm steps init = length $ filter isNeededLeft $ moves gm steps init where
    isNeededLeft (Left e) = e == dr
    isNeededLeft (Right _) = False
----------------------------------------------------------------------------------------------------------------

import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)

{- Не снимайте комментарий - эти объявления даны в вызывающем коде
newtype PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."
-}

instance Monoid PwdError where
    mempty = PwdError ""
    mappend (PwdError x) (PwdError y) = PwdError $ x ++ y
  
getValidPassword :: PwdErrorIOMonad String -- :: ExceptT PwdError IO String
getValidPassword = do
    s <- liftIO getLine
    when (length s < 8) $ showE "Incorrect input: password is too short!"
    when (not $ any isNumber s) $ showE "Incorrect input: password must contain some digits!"
    when (not $ any isPunctuation s) $ showE "Incorrect input: password must contain some punctuation!"
    return s
    where showE s = do {liftIO $ putStrLn s; throwE (PwdError s)}
----------------------------------------------------------------------------------------------------------------

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead "" = throwE $ EmptyInput
tryRead s = case reads s of
    [] -> throwE $ NoParse s
    x:xs -> if (snd x) == "" 
        then return $ fst x 
        else throwE $ NoParse s


----------------------------------------------------------------------------------------------------------------

go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go tr = do
    x <- tryRead tr
    lift $ tell (Sum x)

