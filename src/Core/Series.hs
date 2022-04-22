{-# LANGUAGE MultiParamTypeClasses #-}

module Core.Series (
    Series(..),
    series
) where

class SeriesC a where
    dtype :: a -> String
    --sum :: (Num d, Num a) => a -> d
    
data Index = Index Int

data Series i d = Series {
    seriesData :: [d],
    seriesIndex :: [i],
    seriesName :: String
} deriving (Show, Read, Eq)

series :: (Num i, Enum i) => [d] -> [i] -> String -> Series i d
series dat ind nam = Series {
    seriesData=datum,
    seriesIndex=index,
    seriesName=name
    }
    where
        datum = dat
        index = if null ind then take (length dat) [0..] else ind
        name = if null nam then "column1" else nam

instance SeriesC (Series i d) where
    dtype = undefined
    --sum :: (Num d) => Series i d -> d
    --sum s = Prelude.sum (seriesData s)


    
instance (Monoid i, Monoid d) => Monoid (Series i d) where
    mempty = Series mempty mempty mempty

instance (Semigroup i, Semigroup d) => Semigroup (Series i d) where
    (Series ld li ln) <> (Series rd ri rn) = Series (ld <> rd) (li <> ri) (ln <> rn)
    
instance Functor (Series i) where
    fmap f (Series dat ind nam) = Series (fmap f dat) ind nam
    
instance Applicative (Series i) where
    pure x = Series [x] [] ""
    (Series fd fi fn) <*> (Series sd si sn) = Series (fd <*> sd) [] []
    
instance Monad (Series i) where
    return x = Series [x] [] ""
    (Series dat ind nam) >>= k = Series [] [] []
    
instance MonadFail (Series i) where
    fail _ = Series [] [] ""
    
    