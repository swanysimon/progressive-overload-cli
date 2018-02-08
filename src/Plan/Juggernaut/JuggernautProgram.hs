module Plan.Juggernaut.JuggernautProgram (
        JuggernautProgram(JuggernautProgram)
    ) where

import qualified Util.CycleEnum as Cycle
import Data.Ratio
import Plan.Program
import Plan.Juggernaut.JuggernautPhase
import Plan.Juggernaut.JuggernautWave
import Training.Exercise
import Training.Set
import Training.Units
import Training.Weight
import Training.WorkingSet
import Training.Workout

data JuggernautProgram = JuggernautProgram Exercise Wave Phase Weight
    deriving (Eq, Read, Show)

instance Program JuggernautProgram where
    asWorkout _ = error "Not yet implemented"

    begin exercise workingMax = JuggernautProgram exercise Tens Accumulation workingMax

    next program@(JuggernautProgram e v p w) workout = JuggernautProgram e nextWave nextPhase nextWeight
        where
            nextWave = Cycle.next v
            nextPhase = Cycle.next p
            nextWeight
                    | p == Realization = calculateNewWorkingMax program workout
                    | otherwise = w

calculateNewWorkingMax :: JuggernautProgram -> Workout -> Weight
calculateNewWorkingMax (JuggernautProgram e v _ w@(Weight _ units)) workout
        | cleanedWorkout == [] = error "Cannot calculate new working maximum on an exercise that was not performed"
        | otherwise = adjustWorkingMax estimatedMax calculatedMax
    where
        adjustWorkingMax = addWeights
        cleanedWorkout = map (\s -> convertSetUnits s units) . filter isSetForExercise $ workout
        isSetForExercise (Set exercise _) = exercise == e
        estimatedMax = estimate1Rm realizationSet w v
        realizationSet = last cleanedWorkout
        calculatedMax = calculate1Rm realizationSet

estimate1Rm :: Set -> Weight -> Wave -> Weight
estimate1Rm (Set _ (WorkingSet reps _)) workingMax wave = addWeights workingMax extraCapacity
    where
        extraCapacity = multiplyWeight smallestIncrement extraReps
        extraReps = reps - (expectedRepetitions wave)
        -- TODO: implement a way of configuring this
        smallestIncrement = Weight 5 Pounds

calculate1Rm :: Set -> Weight
calculate1Rm (Set _ (WorkingSet r w)) = addWeights w . flip multiplyWeight scalar . multiplyWeight w $ r
    where
        scalar = 33 % 1000

