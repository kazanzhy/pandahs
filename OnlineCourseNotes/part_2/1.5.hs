module Demo where

----------------------------------------------------------------------------------------------------------------
type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps $ (3 :: Integer, ('x', True)) :: A

b :: B t
b = Cmps $ (True, id, Right 5 :: Either String Int)

c :: C
c  = Cmps $ \x -> if x then (+ (2 :: Integer)) else (+ (3 :: Integer))
----------------------------------------------------------------------------------------------------------------
newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap i (Cmps3 x) = Cmps3 $ fmap (fmap (fmap i)) x
----------------------------------------------------------------------------------------------------------------
unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = fmap getCmps . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = fmap (fmap getCmps) . fmap getCmps . getCmps
----------------------------------------------------------------------------------------------------------------

