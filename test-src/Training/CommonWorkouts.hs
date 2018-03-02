module Training.CommonWorkouts (
        fiveByFiveAtOneThirtyFive
    ) where

import qualified Training.CommonPoundWeights as LB
import Training.Workout

fiveByFiveAtOneThirtyFive :: Exercise -> Workout
fiveByFiveAtOneThirtyFive e = replicate 5 oneThirtyFiveForFive
    where
        oneThirtyFiveForFive = Set e 5 LB.oneThirtyFive

