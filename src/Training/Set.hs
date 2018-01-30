module Training.Set (
        Set(..)
    ) where

import Training.Exercise
import Training.Repetitions
import Training.Weight
import Training.WorkingSet

data Set = Set Exercise WorkingSet
        | DropSet Exercise [WorkingSet]
        | SuperSet [Set]
    deriving (Eq)

