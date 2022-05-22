module Demo where

----------------------------------------------------------------------------------------------------------------
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = sqrt ((x - x')^2 + (y - y')^2)
----------------------------------------------------------------------------------------------------------------
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * (r ^ 2)
area (Rectangle a b) = a * b
----------------------------------------------------------------------------------------------------------------
data Result' = Fail' Int | Success'

instance Show Result' where
    show (Success') = "Success"
    show (Fail' x) = "Fail: " ++ show x

doSomeWork' :: SomeData -> Result'
doSomeWork' inp = case doSomeWork inp of (Success, _) -> Success'
                                         (Fail, x) -> Fail' x
----------------------------------------------------------------------------------------------------------------
data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x y) | x == y = True
                       | otherwise = False
isSquare (Circle r) = False    
----------------------------------------------------------------------------------------------------------------
data Bit = Zero | One 
data Sign = Minus | Plus 
data Z = Z Sign [Bit]

setSign :: Sign -> Int
setSign Minus = (-1)
setSign Plus = 1

getSign :: Int -> Sign
getSign x | x >= 0 = Plus 
          | otherwise = Minus

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

bitsToInt :: [Bit] -> Int
bitsToInt [] = 0
bitsToInt lst = sum (zipWith (\x y -> (bitToInt x) * 2^y) lst [0..])

intToBits :: Int -> [Bit] 
intToBits 0 = [Zero] 
intToBits 1 = [One] 
intToBits n | n `mod` 2 == 0 = Zero : intToBits (n `div` 2) 
            | otherwise      = One : intToBits (n `div` 2)

add :: Z -> Z -> Z
add (Z as a) (Z bs b) = Z (getSign res) (intToBits $ abs res)
      where res = (setSign as) * (bitsToInt a) + (setSign bs) * (bitsToInt b)

mul :: Z -> Z -> Z
mul (Z as a) (Z bs b) = Z (getSign res) (intToBits $ abs res)
     where res = (setSign as) * (bitsToInt a) * (setSign bs) * (bitsToInt b)
----------------------------------------------------------------------------------------------------------------

