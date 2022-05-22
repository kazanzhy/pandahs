module Demo where

----------------------------------------------------------------------------------------------------------------
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = case f x of Log msgb b -> case g b of Log msgc c -> Log (msgb ++ msgc) c
----------------------------------------------------------------------------------------------------------------
returnLog :: a -> Log a
returnLog = Log []
----------------------------------------------------------------------------------------------------------------
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg x) g = Log (msg ++ ms) res where Log ms res = g x
----------------------------------------------------------------------------------------------------------------
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList ini lst = foldl (>>=) (return ini) lst 
----------------------------------------------------------------------------------------------------------------

