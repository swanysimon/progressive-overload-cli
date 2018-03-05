module Plan.Juggernaut.JuggernautMethodSpec (
        main, -- only here so module can be run from ghci for debugging
        spec
    ) where

import qualified Training.CommonKilogramWeights as KG
import qualified Training.CommonPoundWeights as LB
import qualified Training.CommonWorkouts as Workouts
import Plan.Program
import Plan.Juggernaut.JuggernautMethod
import Plan.Juggernaut.JuggernautPhase
import Plan.Juggernaut.JuggernautWave
import Test.Hspec
import Training.Weight

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
        describe "instance of Program" programInstanceTests
        describe "function calculateNewWorkingMax" calculateNewWorkingMaxTests

programInstanceTests :: SpecWith ()
programInstanceTests = do
        it "tests JuggernautMethod instance of Program.begin" test_program_begin
        it "tests JuggernautMethod instance of Program.next" test_program_next
        it "tests JuggernautMethod instance of Program.asWorkout" test_program_as_workout

test_program_begin :: Expectation
test_program_begin = do
        begin "" KG.zero minJump `shouldBe` JuggernautMethod "" Tens Accumulation KG.zero minJump
        begin "squat" KG.zero minJump `shouldBe` JuggernautMethod "squat" Tens Accumulation KG.zero minJump
        begin "bench" KG.forty minJump `shouldBe` JuggernautMethod "bench" Tens Accumulation KG.forty minJump
        begin "asdfjkl;" LB.fortyFourPointOne minJump `shouldBe` JuggernautMethod "asdfjkl;" Tens Accumulation LB.fortyFourPointOne minJump

minJump :: MinimumWeightJump
minJump = Weight 5 Pounds

test_program_next :: Expectation
test_program_next = do
        next tensAccumulation fiveByFive `shouldBe` tensIntensification
        next tensRealization fiveByFive `shouldBe` JuggernautMethod exercise Tens Deload (Weight (-25) Pounds) minJump
        next tensDeload fiveByFive `shouldBe` eightsAccumulation
        next eightsDeload fiveByFive `shouldBe` fivesAccumulation
        next fivesDeload fiveByFive `shouldBe` threesAccumulation
        next threesDeload fiveByFive `shouldBe` tensAccumulation
    where
        tensAccumulation = JuggernautMethod exercise Tens Accumulation zeroWeight minJump
        exercise = "squat"
        zeroWeight = LB.zero
        fiveByFive = Workouts.fiveByFiveAtOneThirtyFive exercise
        tensIntensification = JuggernautMethod exercise Tens Intensification zeroWeight minJump
        tensRealization = JuggernautMethod exercise Tens Realization zeroWeight minJump
        tensDeload = JuggernautMethod exercise Tens Deload zeroWeight minJump
        eightsAccumulation = JuggernautMethod exercise Eights Accumulation zeroWeight minJump
        eightsDeload = JuggernautMethod exercise Eights Deload zeroWeight minJump
        fivesAccumulation = JuggernautMethod exercise Fives Accumulation zeroWeight minJump
        fivesDeload = JuggernautMethod exercise Fives Deload zeroWeight minJump
        threesAccumulation = JuggernautMethod exercise Threes Accumulation zeroWeight minJump
        threesDeload = JuggernautMethod exercise Threes Deload zeroWeight minJump

test_program_as_workout :: Expectation
test_program_as_workout = do
        asWorkout tensRealization `shouldBe` tensRealizationWorkout
        asWorkout eightsRealization `shouldBe` eightsRealizationWorkout
        asWorkout eightsIntensification `shouldBe` eightsIntensificationWorkout
        asWorkout threesDeload `shouldBe` threesDeloadWorkout
    where
        tensRealization = JuggernautMethod exercise Tens Realization LB.oneThousand minJump
        exercise = "bench press"
        tensRealizationWorkout = [
                calculateSet exercise 5 (Weight 500 Pounds) 1 minJump,
                calculateSet exercise 3 (Weight 600 Pounds) 1 minJump,
                calculateSet exercise 1 (Weight 700 Pounds) 1 minJump,
                calculateSet exercise 0 (Weight 750 Pounds) 1 minJump]
        eightsRealization = JuggernautMethod exercise Eights Realization LB.oneThousand minJump
        eightsRealizationWorkout = [
                calculateSet exercise 5 (Weight 500 Pounds) 1 minJump,
                calculateSet exercise 3 (Weight 600 Pounds) 1 minJump,
                calculateSet exercise 2 (Weight 700 Pounds) 1 minJump,
                calculateSet exercise 1 (Weight 750 Pounds) 1 minJump,
                calculateSet exercise 0 (Weight 800 Pounds) 1 minJump]
        eightsIntensification = JuggernautMethod exercise Eights Intensification KG.oneThousand minJump
        eightsIntensificationWorkout = [
                calculateSet exercise 3 (Weight 600 Kilograms) 1 minJump,
                calculateSet exercise 3 (Weight 675 Kilograms) 1 minJump,
                calculateSet exercise 8 (Weight 725 Kilograms) 1 minJump,
                calculateSet exercise 8 (Weight 725 Kilograms) 1 minJump,
                calculateSet exercise 0 (Weight 725 Kilograms) 1 minJump]
        threesDeload = JuggernautMethod exercise Threes Deload (Weight 11.4529 Kilograms) minJump
        threesDeloadWorkout = [
                calculateSet exercise 5 (Weight 4.58116 Kilograms) 1 minJump,
                calculateSet exercise 5 (Weight 5.72645 Kilograms) 1 minJump,
                calculateSet exercise 5 (Weight 6.87174 Kilograms) 1 minJump]

calculateNewWorkingMaxTests :: SpecWith ()
calculateNewWorkingMaxTests = do
        it "tests weight increases by minimum weight increase times reps above expected" test_working_max_increase
        it "tests limits to 95% of calculated 1RM" test_limit_working_max

test_working_max_increase :: Expectation
test_working_max_increase = do
        next tensRealization [fifteenReps] `shouldBe` tensDeload (Weight 1025 Pounds)
        next tensRealization [twentyReps] `shouldBe` tensDeload (Weight 1050 Pounds)
        next eightsRealization [thirteenReps] `shouldBe` eightsDeload (Weight 1025 Pounds)
        next threesRealization [sevenReps] `shouldBe` threesDeload (Weight 1020 Pounds)
    where
        tensRealization = JuggernautMethod exercise Tens Realization LB.oneThousand minJump
        exercise = "deadlift"
        fifteenReps = calculateSet exercise 15 tensRealizationWeight 1 minJump
        tensRealizationWeight = Weight 750 Pounds
        tensDeload workingMax = JuggernautMethod exercise Tens Deload workingMax minJump
        twentyReps = calculateSet exercise 20 tensRealizationWeight 1 minJump
        eightsRealization = JuggernautMethod exercise Eights Realization LB.oneThousand minJump
        thirteenReps = calculateSet exercise 13 (Weight 800 Pounds) 1 minJump
        eightsDeload workingMax = JuggernautMethod exercise Eights Deload workingMax minJump
        threesRealization = JuggernautMethod exercise Threes Realization LB.oneThousand minJump
        sevenReps = calculateSet exercise 7 (Weight 900 Pounds) 1 minJump
        threesDeload workingMax = JuggernautMethod exercise Threes Deload workingMax minJump

test_limit_working_max :: Expectation
test_limit_working_max = do
        next tensRealization [fifteenReps] `shouldBe` tensDeload (Weight 408.5 Pounds)
        next fivesRealization [eightReps] `shouldBe` fivesDeload (Weight 969 Pounds)
    where
        tensRealization = JuggernautMethod exercise Tens Realization (Weight 400 Pounds) minJump
        exercise = "overhead press"
        fifteenReps = calculateSet exercise 13 (Weight 300 Pounds) 1 minJump
        tensDeload workingMax = JuggernautMethod exercise Tens Deload workingMax minJump
        fivesRealization = JuggernautMethod exercise Fives Realization LB.oneThousand minJump
        eightReps = calculateSet exercise 6 (Weight 850 Pounds) 1 minJump
        fivesDeload workingMax = JuggernautMethod exercise Fives Deload workingMax minJump

