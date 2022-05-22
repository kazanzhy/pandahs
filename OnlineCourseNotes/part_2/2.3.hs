module Demo where

----------------------------------------------------------------------------------------------------------------
instance Functor OddC where
    fmap f (Un x) = Un (f x)
    fmap f (Bi x y od) = Bi (f x) (f y) (fmap f od)

instance Applicative OddC where
    pure x = Un x
    (Bi f g h) <*> (Bi x y od) = Bi (f x) (g y) (h <*> od)
    (Un f) <*> (Bi x y od) = Bi (f x) (f y) (f <$> od)
    (Bi f g h) <*> (Un x) = Bi (f x) (g x) (h <*> pure x)
    (Un f) <*> (Un x) = Un (f x)
    
instance Foldable OddC where
    foldr f ini (Un x) = f x ini
    foldr f ini (Bi x y od) = f x (f y (foldr f ini od))
      
instance Traversable OddC where
    traverse g (Un x) = pure Un <*> g x
    traverse g (Bi x y od) = pure Bi <*> g x <*> g y <*> (traverse g od)
----------------------------------------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Temperature a = Temperature Double
  deriving (Num,Fractional,Eq,Show)

data Celsius
data Fahrenheit 
data Kelvin 

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature (k - 273.15)
----------------------------------------------------------------------------------------------------------------
import Data.Traversable (foldMapDefault)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  sequenceA Nil = pure Nil
  sequenceA (Branch l x r) = pure flip <*> (pure Branch <*> sequenceA l) <*> sequenceA r <*> x
----------------------------------------------------------------------------------------------------------------

