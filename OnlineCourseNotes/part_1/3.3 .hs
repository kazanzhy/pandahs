module Demo where

----------------------------------------------------------------------------------------------------------------
fibStream :: [Integer]
fibStream = 0: 1 : zipWith (+) fibStream (tail fibStream)
----------------------------------------------------------------------------------------------------------------
repeatHelper = id
----------------------------------------------------------------------------------------------------------------
instance Enum Odd where
    --toEnum (Odd x) = x
    --fromEnum (x) = Odd x  
    
    succ (Odd x) = Odd (x + 2)
          
    pred (Odd x) = Odd (x - 2)

    enumFrom (Odd x) = (Odd x) : enumFrom (Odd (x + 2))

    enumFromTo (Odd x) (Odd z) | x > z = []
		                       | otherwise = (Odd x) : enumFromTo (Odd (x + 2)) (Odd z)

    enumFromThen (Odd x) (Odd y) = (Odd x) : enumFromThen (Odd y) (Odd (y + step)) where step = y - x

    enumFromThenTo (Odd x) (Odd y) (Odd z) | (x > z) && (step > 0) = []
                                           | (x < z) && (step < 0) = []
                                           | otherwise = (Odd x) : enumFromThenTo (Odd y) (Odd (y + step)) (Odd z) 
                                               where step = y - x
----------------------------------------------------------------------------------------------------------------
change :: (Ord a, Num a) => a -> [[a]]

change s | s < minimum coins = []
         | otherwise = [xs | xs <- [ n : y | n <- coins, y <- [] : (change $ s - n) ], sum xs == s]
----------------------------------------------------------------------------------------------------------------
