module Demo where

----------------------------------------------------------------------------------------------------------------
class Printable a where
    toString :: a -> [Char]
    
instance Printable Bool where
    toString True = "true"
    toString False = "false"
    
instance Printable () where
    toString () = "unit type"
----------------------------------------------------------------------------------------------------------------
instance (Printable a, Printable b) => Printable (a,b) where
    toString p = "(" ++ (toString $ fst p) ++ "," ++ (toString $ snd p) ++ ")"
----------------------------------------------------------------------------------------------------------------

