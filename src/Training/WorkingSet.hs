module Training.WorkingSet (
        WorkingSet(WorkingSet)
    ) where

import Data.Ratio
import Training.Repetitions
import Training.Weight

data WorkingSet = WorkingSet Repetitions Weight
    deriving (Eq)

