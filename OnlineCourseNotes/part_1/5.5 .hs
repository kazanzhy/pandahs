module Demo where

----------------------------------------------------------------------------------------------------------------
main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr "Name: "
    hFlush stdout
    name <- getLine
    let out = "Hi, " ++ name ++ "!"
    if name == "" then main' else (putStrLn out)
----------------------------------------------------------------------------------------------------------------
import Data.List (isInfixOf)

main' :: IO ()
main' = do 
    putStr "Substring: "
    hFlush stdout
    inp <- getLine
    if null inp then putStrLn "Canceled" 
    else do
        dir_files <- getDirectoryContents "."
        let filtered_files = filter (\x -> isInfixOf inp x) dir_files
        deleted_files <- mapM removeFile filtered_files
        let print_files = map (\f -> "Removing file: " ++ f) filtered_files
        mapM putStrLn print_files
        return ()
----------------------------------------------------------------------------------------------------------------

