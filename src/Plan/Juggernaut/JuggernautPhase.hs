module Plan.Juggernaut.JuggernautPhase (
        Phase(Accumulation, Intensification, Realization, Deload)
    ) where

import Util.CycleEnum

data Phase = Accumulation
        | Intensification
        | Realization
        | Deload
    deriving (Bounded, Enum, Eq, Read, Show)

instance CycleEnum Phase where

