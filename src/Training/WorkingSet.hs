module Training.WorkingSet (
        WorkingSet
    ) where

import Training.Repetitions
import Training.Weight

data WorkingSet = WorkingSet Repetitions Weight
    deriving (Eq)

