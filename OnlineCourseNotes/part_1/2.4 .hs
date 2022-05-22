module Demo where

----------------------------------------------------------------------------------------------------------------
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a && doesEnrageGork a = (stomp . stab) a
                  | doesEnrageMork a = stomp a
                  | doesEnrageGork a = stab a
                  | otherwise = a
----------------------------------------------------------------------------------------------------------------
a = 127.2
b = 24.1
c = 20.1
d = 2
----------------------------------------------------------------------------------------------------------------
class (Eq a, Bounded a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x = if (x == maxBound) then (minBound) else (succ x)
  
  spred :: a -> a
  spred y = if (y == minBound) then (maxBound) else (pred y)
----------------------------------------------------------------------------------------------------------------.
avg :: Int -> Int -> Int -> Double
avg x y z = ((fromIntegral x + fromIntegral y + fromIntegral z) / 3) :: Double
----------------------------------------------------------------------------------------------------------------
