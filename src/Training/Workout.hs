module Training.Workout (
        Exercise,
        Repetitions,
        Set(..),
        Workout,
        convertSetUnits
    ) where

import Training.Weight

type Exercise = String
type Repetitions = Integer
type Workout = [Set]

data Set = Set Exercise Repetitions Weight
    deriving (Eq, Read, Show)

convertSetUnits :: Set -> Units -> Set
convertSetUnits (Set exercise reps weight) = Set exercise reps . convertWeightUnits weight

