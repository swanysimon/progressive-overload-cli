module Plan.Juggernaut.JuggernautMethod (
        JuggernautMethod(..),
        calculateSet -- visible for testing
    ) where

import qualified Plan.CommonPercentages as Percentage
import qualified Util.CycleEnum as Cycle
import Plan.Program
import Plan.Juggernaut.JuggernautPhase
import Plan.Juggernaut.JuggernautWave
import Training.Weight
import Training.Workout

data JuggernautMethod = JuggernautMethod {
        getExercise :: Exercise,
        getWave :: Wave,
        getPhase :: Phase,
        getWorkingMax :: WorkingMax,
        getMinimumWeightJump :: MinimumWeightJump
    } deriving (Eq, Read, Show)

instance Program JuggernautMethod where

    asWorkout (JuggernautMethod exercise wave phase workingMax minJump) = case phase of
            Accumulation -> toWorkout accumulationWorkout
            Intensification -> toWorkout intensificationWorkout
            Realization -> toWorkout realizationWorkout
            Deload -> toWorkout deload
        where
            toWorkout workoutFunction = workoutFunction exercise workingMax minJump
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

    begin exercise workingMax minJump@(Weight _ units) = JuggernautMethod exercise Tens Accumulation convertedWorkingMax minJump
        where
            convertedWorkingMax = convertWeightUnits workingMax units

    next program@(JuggernautMethod exercise wave phase workingMax minJump) workout = case phase of
            Realization -> JuggernautMethod exercise wave nextPhase newWorkingMax minJump
            Deload -> juggernautWithWave nextWave
            _ -> juggernautWithWave wave
        where
            nextPhase = Cycle.next phase
            newWorkingMax = calculateNewWorkingMax program workout
            juggernautWithWave newWave = JuggernautMethod exercise newWave nextPhase workingMax minJump
            nextWave = Cycle.next wave

tensAccumulationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
tensAccumulationWorkout exercise workingMax = lastSetAsAmrap . replicate 5 . calculateSet exercise 10 workingMax Percentage.sixty

calculateSet :: Exercise -> Repetitions -> WorkingMax -> Rational -> MinimumWeightJump -> Set
calculateSet exercise repsToPerform workingMax percentage minJump = roundSetToMinimumWeightJump calculatedSet minJump
    where
        -- TODO: figure this out
        roundSetToMinimumWeightJump s m = s
        calculatedSet = Set exercise repsToPerform workingWeight
        workingWeight = multiplyWeight workingMax percentage

lastSetAsAmrap :: Workout -> Workout
lastSetAsAmrap ((Set e _ w):[]) = [Set e 0 w]
lastSetAsAmrap (x:xs) = x : lastSetAsAmrap xs
lastSetAsAmrap _ = []

eightsAccumulationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
eightsAccumulationWorkout exercise workingMax = lastSetAsAmrap . replicate 5 . calculateSet exercise 8 workingMax Percentage.sixtyFive

fivesAccumulationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
fivesAccumulationWorkout exercise workingMax = lastSetAsAmrap . replicate 6 . calculateSet exercise 5 workingMax Percentage.seventy

threesAccumulationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
threesAccumulationWorkout exercise workingMax = lastSetAsAmrap . replicate 7 . calculateSet exercise 3 workingMax Percentage.seventyFive

tensIntensificationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
tensIntensificationWorkout exercise workingMax minJump = fiftyFiveForFive : sixtyTwoForFive : workingSets minJump
    where
        fiftyFiveForFive = calculateSet exercise 5 workingMax Percentage.fiftyFive minJump
        sixtyTwoForFive = calculateSet exercise 5 workingMax Percentage.sixtyTwoPointFive minJump
        workingSets = lastSetAsAmrap . replicate 3 . calculateSet exercise 10 workingMax Percentage.sixtySevenPointFive

eightsIntensificationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
eightsIntensificationWorkout exercise workingMax minJump = sixtyForThree : sixtySevenPointFiveForThree : workingSets minJump
    where
        sixtyForThree = calculateSet exercise 3 workingMax Percentage.sixty minJump
        sixtySevenPointFiveForThree = calculateSet exercise 3 workingMax Percentage.sixtySevenPointFive minJump
        workingSets = lastSetAsAmrap . replicate 3 . calculateSet exercise 8 workingMax Percentage.seventyTwoPointFive

fivesIntensificationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
fivesIntensificationWorkout exercise workingMax minJump = sixtyFiveForTwo : seventyTwoPointFiveForTwo : workingSets minJump
    where
        sixtyFiveForTwo = calculateSet exercise 2 workingMax Percentage.sixtyFive minJump
        seventyTwoPointFiveForTwo = calculateSet exercise 2 workingMax Percentage.seventyTwoPointFive minJump
        workingSets = lastSetAsAmrap . replicate 4 . calculateSet exercise 5 workingMax Percentage.seventySevenPointFive

threesIntensificationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
threesIntensificationWorkout exercise workingMax minJump = seventyForOne : seventySevenPointFiveForOne : workingSets minJump
    where
        seventyForOne = calculateSet exercise 1 workingMax Percentage.seventy minJump
        seventySevenPointFiveForOne = calculateSet exercise 1 workingMax Percentage.seventySevenPointFive minJump
        workingSets = lastSetAsAmrap . replicate 5 . calculateSet exercise 3 workingMax Percentage.eightyTwoPointFive

tensRealizationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
tensRealizationWorkout exercise workingMax minJump = lastSetAsAmrap . (++) ys . firstElementForOne $ zs
    where
        (ys,zs) = splitAt 2 . take 4 . realizationRampUp exercise workingMax $ minJump
        firstElementForOne [] = []
        firstElementForOne ((Set _ _ w):xs) = (Set exercise 1 w) : xs

realizationRampUp :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
realizationRampUp exercise workingMax minJump = fiftyForFive
        : sixtyForThree
        : seventyForTwo
        : seventyFiveForOne
        : eightyForOne
        : eightyFiveForOne
        : ninetyForOne
        : []
    where
        fiftyForFive = calculateSet exercise 5 workingMax Percentage.fifty minJump
        sixtyForThree = calculateSet exercise 3 workingMax Percentage.sixty minJump
        seventyForTwo = calculateSet exercise 2 workingMax Percentage.seventy minJump
        seventyFiveForOne = calculateSet exercise 1 workingMax Percentage.seventyFive minJump
        eightyForOne = calculateSet exercise 1 workingMax Percentage.eighty minJump
        eightyFiveForOne = calculateSet exercise 1 workingMax Percentage.eightyFive minJump
        ninetyForOne = calculateSet exercise 1 workingMax Percentage.ninety minJump

eightsRealizationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
eightsRealizationWorkout exercise workingMax = lastSetAsAmrap . take 5 . realizationRampUp exercise workingMax

fivesRealizationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
fivesRealizationWorkout exercise workingMax = lastSetAsAmrap . take 6 . realizationRampUp exercise workingMax

threesRealizationWorkout :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
threesRealizationWorkout exercise workingMax = lastSetAsAmrap . take 7 . realizationRampUp exercise workingMax

deload :: Exercise -> WorkingMax -> MinimumWeightJump -> Workout
deload exercise workingMax minJump = fortyForFive : fiftyForFive : sixtyForFive : []
    where
        fortyForFive = calculateSet exercise 5 workingMax Percentage.forty minJump
        fiftyForFive = calculateSet exercise 5 workingMax Percentage.fifty minJump
        sixtyForFive = calculateSet exercise 5 workingMax Percentage.sixty minJump

calculateNewWorkingMax :: JuggernautMethod -> Workout -> Weight
calculateNewWorkingMax (JuggernautMethod exercise wave _ workingMax minJump@(Weight _ units)) workout
        | cleanedWorkout == [] = error "Cannot calculate new working maximum on an exercise that was not performed"
        | otherwise = adjustWorkingMax calculatedMax calculatedWorkingMax
    where
        cleanedWorkout = map (\s -> convertSetUnits s units) . filter isSetForExercise $ workout
        isSetForExercise (Set e _ _) = e == exercise
        calculatedMax = calculate1Rm realizationSet
        realizationSet = last cleanedWorkout
        calculatedWorkingMax = estimateWorkingMax realizationSet workingMax minJump wave

{-
 - The Juggernaut Method states that you should adjust your estimated working max to never be
 - more than 95% of your calculated one rep max
 -}
adjustWorkingMax :: Weight -> Weight -> Weight
adjustWorkingMax calculatedMax@(Weight _ units) calculatedWorkingMax@(Weight _ otherUnits)
        | units /= otherUnits = adjustWorkingMax calculatedMax . convertWeightUnits calculatedWorkingMax $ units
        | otherwise = min calculatedWorkingMax . multiplyWeight calculatedMax $ Percentage.ninetyFive

estimateWorkingMax :: Set -> WorkingMax -> MinimumWeightJump -> Wave -> Weight
estimateWorkingMax (Set _ reps _) workingMax minJump wave = addWeights workingMax extraCapacity
    where
        extraCapacity = multiplyWeight minJump extraReps
        extraReps = (-) reps . expectedRepetitions $ wave

calculate1Rm :: Set -> Weight
calculate1Rm (Set _ 1 weight) = weight
calculate1Rm (Set _ reps weight) = addWeights weight extraCapacity
    where
        extraCapacity = multiplyWeight weight . (*) Percentage.oneRepMaxVolumeScalar . toRational $ reps

