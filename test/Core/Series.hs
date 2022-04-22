module Core.Series where

import Core.Series
import qualified Prelude

test :: Series Int Int
test = Series {
    seriesData = [1,2,3], 
    seriesIndex = [1,2,3],
    seriesName = "test"
    }