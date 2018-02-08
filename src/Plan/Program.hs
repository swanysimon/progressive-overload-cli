module Plan.Program (
        Program,
        asWorkout,
        begin,
        next
    ) where

import Training.Exercise
import Training.Weight
import Training.Workout

class Program p where
    asWorkout :: p -> Workout

    begin :: Exercise -> Weight -> p

    next :: p -> Workout -> p

