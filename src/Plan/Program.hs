module Plan.Program (
        getWorkout,
        getFromPreviousWorkout,
        start,
        update
    ) where

import Training.Workout

class Program p where
    getWorkout :: p -> Workout
    getFromPreviousWorkout :: p -> Workout -> Workout
    start :: p -> IO ()
    update :: p -> IO ()

