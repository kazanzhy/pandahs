module Core.Frame (
    DataFrame(..)
) where

data DataFrame i d = DataFrame {
    seriesData :: [d],
    seriesIndex :: [i],
    seriesDtype :: String,
    seriesName :: String,
    seriesCopy :: Bool,
    seriesFastpath :: Bool
} deriving (Show, Read, Eq)