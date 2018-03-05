module Plan.Program (
        Program,
        asWorkout,
        begin,
        next
    ) where

import Training.Weight
import Training.Workout

class Program p where
    asWorkout :: p -> Workout

    begin :: Exercise -> WorkingMax -> MinimumWeightJump -> p

    next :: p -> Workout -> p

