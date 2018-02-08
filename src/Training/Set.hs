module Training.Set (
        Set(Set),
        convertSetUnits
    ) where

import Training.Exercise
import Training.Units
import Training.Weight
import Training.WorkingSet

data Set = Set Exercise WorkingSet
    deriving (Eq)

convertSetUnits :: Set -> Units -> Set
convertSetUnits (Set e (WorkingSet reps w)) = Set e . WorkingSet reps . convertWeightUnits w

