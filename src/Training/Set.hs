module Training.Set (
        Set(..)
    ) where

import Training.Exercise
import Training.WorkingSet

data Set = Set Exercise WorkingSet
        | DropSet Exercise [WorkingSet]
        | SuperSet [Set]
    deriving (Eq)

