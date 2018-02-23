module Training.CommonWorkouts (
        fiveByFiveAtOneThirtyFive
    ) where

import qualified Training.CommonPoundWeights as LB
import Training.Exercise
import Training.WorkingSet
import Training.Workout
import Training.Set

fiveByFiveAtOneThirtyFive :: Exercise -> Workout
fiveByFiveAtOneThirtyFive e = replicate 5 . Set e . WorkingSet 5 $ LB.oneThirtyFive

