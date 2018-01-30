module Training.Workout (
        Workout,
    ) where

import Training.Set

newtype Workout = Workout [Set]
    deriving (Eq)

