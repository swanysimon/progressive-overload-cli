module Plan.Juggernaut.JuggernautMethod (
        JuggernautMethod(JuggernautMethod)
    ) where

import qualified Plan.CommonPercentages as Percentage
import qualified Util.CycleEnum as Cycle
import Plan.Program
import Plan.Juggernaut.JuggernautPhase
import Plan.Juggernaut.JuggernautWave
import Training.Exercise
import Training.Set
import Training.Units
import Training.Weight
import Training.WorkingSet
import Training.Workout

data JuggernautMethod = JuggernautMethod Exercise Wave Phase Weight
    deriving (Eq, Read, Show)

instance Program JuggernautMethod where

    asWorkout (JuggernautMethod exercise wave phase weight) = case phase of
            Accumulation -> accumulationWorkout exercise weight
            Intensification -> intensificationWorkout exercise weight
            Realization -> realizationWorkout exercise weight
            Deload -> deload exercise weight
        where
            accumulationWorkout = case wave of
                    Tens -> tensAccumulationWorkout
                    Eights -> eightsAccumulationWorkout
                    Fives -> fivesAccumulationWorkout
                    Threes -> threesAccumulationWorkout
            intensificationWorkout = case wave of
                    Tens -> tensIntensificationWorkout
                    Eights -> eightsIntensificationWorkout
                    Fives -> fivesIntensificationWorkout
                    Threes -> threesIntensificationWorkout
            realizationWorkout = case wave of
                    Tens -> tensRealizationWorkout
                    Eights -> eightsRealizationWorkout
                    Fives -> fivesRealizationWorkout
                    Threes -> threesRealizationWorkout

    begin exercise workingMax = JuggernautMethod exercise Tens Accumulation workingMax

    next program@(JuggernautMethod e v p w) workout = JuggernautMethod e nextWave nextPhase nextWeight
        where
            nextWave = Cycle.next v
            nextPhase = Cycle.next p
            nextWeight
                    | p == Realization = calculateNewWorkingMax program workout
                    | otherwise = w

tensAccumulationWorkout :: Exercise -> Weight -> Workout
tensAccumulationWorkout e weight = lastSetAsAmrap . replicate 5 . calculateSet e 10 weight $ Percentage.sixty

lastSetAsAmrap :: Workout -> Workout
lastSetAsAmrap ((Set e (WorkingSet _ w)):[]) = [Set e . WorkingSet 0 $ w]
lastSetAsAmrap (x:xs) = x : lastSetAsAmrap xs
lastSetAsAmrap _ = []

eightsAccumulationWorkout :: Exercise -> Weight -> Workout
eightsAccumulationWorkout e weight = lastSetAsAmrap . replicate 5 . calculateSet e 8 weight $ Percentage.sixtyFive

fivesAccumulationWorkout :: Exercise -> Weight -> Workout
fivesAccumulationWorkout e weight = lastSetAsAmrap . replicate 6 . calculateSet e 5 weight $ Percentage.seventy

threesAccumulationWorkout :: Exercise -> Weight -> Workout
threesAccumulationWorkout e weight = lastSetAsAmrap . replicate 7 . calculateSet e 3 weight $ Percentage.seventyFive

tensIntensificationWorkout :: Exercise -> Weight -> Workout
tensIntensificationWorkout e weight = fiftyFiveForFive : sixtyTwoForFive : workingSets
    where
        fiftyFiveForFive = calculateSet e 5 weight Percentage.fiftyFive
        sixtyTwoForFive = calculateSet e 5 weight Percentage.sixtyTwoPointFive
        workingSets = lastSetAsAmrap . replicate 3 . calculateSet e 10 weight $ Percentage.sixtySevenPointFive

eightsIntensificationWorkout :: Exercise -> Weight -> Workout
eightsIntensificationWorkout e weight = sixtyForThree : sixtySevenPointFiveForThree : workingSets
    where
        sixtyForThree = calculateSet e 3 weight Percentage.sixty
        sixtySevenPointFiveForThree = calculateSet e 3 weight Percentage.sixtySevenPointFive
        workingSets = lastSetAsAmrap . replicate 3 . calculateSet e 8 weight $ Percentage.seventyTwoPointFive

fivesIntensificationWorkout :: Exercise -> Weight -> Workout
fivesIntensificationWorkout e weight = sixtyFiveForTwo : seventyTwoPointFiveForTwo : workingSets
    where
        sixtyFiveForTwo = calculateSet e 2 weight Percentage.sixtyFive
        seventyTwoPointFiveForTwo = calculateSet e 2 weight Percentage.seventyTwoPointFive
        workingSets = lastSetAsAmrap . replicate 4 . calculateSet e 5 weight $ Percentage.seventySevenPointFive

threesIntensificationWorkout :: Exercise -> Weight -> Workout
threesIntensificationWorkout e weight = seventyForOne : seventySevenPointFiveForOne : workingSets
    where
        seventyForOne = calculateSet e 1 weight Percentage.seventy
        seventySevenPointFiveForOne = calculateSet e 1 weight Percentage.seventySevenPointFive
        workingSets = lastSetAsAmrap . replicate 5 . calculateSet e 3 weight $ Percentage.eightyTwoPointFive

tensRealizationWorkout :: Exercise -> Weight -> Workout
tensRealizationWorkout e weight = lastSetAsAmrap . (++) ys . firstElementForOne $ zs
    where
        (ys,zs) = splitAt 3 . take 4 . realizationRampUp e $ weight
        firstElementForOne [] = []
        firstElementForOne ((Set _ (WorkingSet _ w)):xs) = (Set e (WorkingSet 1 w)) : xs

realizationRampUp :: Exercise -> Weight -> Workout
realizationRampUp e weight = fiftyForFive
        : sixtyForThree
        : seventyForTwo
        : seventyFiveForOne
        : eightyForOne
        : eightyFiveForOne
        : ninetyForOne
        : []
    where
        fiftyForFive = calculateSet e 5 weight Percentage.fifty
        sixtyForThree = calculateSet e 3 weight Percentage.sixty
        seventyForTwo = calculateSet e 2 weight Percentage.seventy
        seventyFiveForOne = calculateSet e 1 weight Percentage.seventyFive
        eightyForOne = calculateSet e 1 weight Percentage.eighty
        eightyFiveForOne = calculateSet e 1 weight Percentage.eightyFive
        ninetyForOne = calculateSet e 1 weight Percentage.ninety

eightsRealizationWorkout :: Exercise -> Weight -> Workout
eightsRealizationWorkout e = lastSetAsAmrap . take 5 . realizationRampUp e

fivesRealizationWorkout :: Exercise -> Weight -> Workout
fivesRealizationWorkout e = lastSetAsAmrap . take 6 . realizationRampUp e

threesRealizationWorkout :: Exercise -> Weight -> Workout
threesRealizationWorkout e = lastSetAsAmrap . take 7 . realizationRampUp e

deload :: Exercise -> Weight -> Workout
deload e weight = fortyForFive : fiftyForFive : sixtyForFive : []
    where
        fortyForFive = calculateSet e 5 weight Percentage.forty
        fiftyForFive = calculateSet e 5 weight Percentage.fifty
        sixtyForFive = calculateSet e 5 weight Percentage.sixty

calculateNewWorkingMax :: JuggernautMethod -> Workout -> Weight
calculateNewWorkingMax (JuggernautMethod e v _ w@(Weight _ units)) workout
        | cleanedWorkout == [] = error "Cannot calculate new working maximum on an exercise that was not performed"
        | otherwise = adjustWorkingMax calculatedMax calculatedWorkingMax
    where
        cleanedWorkout = map (\s -> convertSetUnits s units) . filter isSetForExercise $ workout
        isSetForExercise (Set exercise _) = exercise == e
        calculatedMax = calculate1Rm realizationSet
        realizationSet = last cleanedWorkout
        calculatedWorkingMax = estimateWorkingMax realizationSet w v

{-
 - The Juggernaut Program states that you should adjust your estimated working max to never be
 - more than 95% of your calculated one rep max
 -}
adjustWorkingMax :: Weight -> Weight -> Weight
adjustWorkingMax calculatedMax@(Weight _ units) calculatedWorkingMax@(Weight _ otherUnits)
        | units /= otherUnits = adjustWorkingMax calculatedMax . convertWeightUnits calculatedWorkingMax $ units
        | otherwise = min calculatedWorkingMax . multiplyWeight calculatedMax $ Percentage.ninetyFive

estimateWorkingMax :: Set -> Weight -> Wave -> Weight
estimateWorkingMax (Set _ (WorkingSet reps _)) workingMax wave = addWeights workingMax extraCapacity
    where
        extraCapacity = multiplyWeight smallestIncrement extraReps
        extraReps = (-) reps . expectedRepetitions $ wave
        -- TODO: implement a way of configuring this
        smallestIncrement = Weight 5 Pounds

calculate1Rm :: Set -> Weight
calculate1Rm (Set _ (WorkingSet reps weight)) = addWeights weight extraCapacity
    where
        extraCapacity = multiplyWeight weight . (*) Percentage.oneRepMaxVolumeScalar . toRational $ reps

