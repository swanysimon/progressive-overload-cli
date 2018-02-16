module Training.Set (
        Set(Set),
        calculateSet,
        convertSetUnits
    ) where

import Training.Exercise
import Training.Units
import Training.Weight
import Training.WorkingSet

data Set = Set Exercise WorkingSet
    deriving (Eq)

calculateSet :: Exercise -> Integer -> Weight -> Rational -> Set
calculateSet exercise reps weight percentage = Set exercise . WorkingSet reps . multiplyWeight weight $ percentage

convertSetUnits :: Set -> Units -> Set
convertSetUnits (Set e (WorkingSet reps w)) = Set e . WorkingSet reps . convertWeightUnits w

