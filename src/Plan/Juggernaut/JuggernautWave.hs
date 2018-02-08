module Plan.Juggernaut.JuggernautWave (
        Wave(Tens, Eights, Fives, Threes),
        expectedRepetitions
    ) where

import Training.Repetitions
import Util.CycleEnum

data Wave = Tens
        | Eights
        | Fives
        | Threes
    deriving (Bounded, Enum, Eq, Read, Show)

instance CycleEnum Wave where

expectedRepetitions :: Wave -> Repetitions
expectedRepetitions Tens = 10
expectedRepetitions Eights = 8
expectedRepetitions Fives = 5
expectedRepetitions Threes = 3

