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
import Training.Set
import Training.Units
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
        begin "" KG.zero `shouldBe` JuggernautMethod "" Tens Accumulation KG.zero
        begin "squat" KG.zero `shouldBe` JuggernautMethod "squat" Tens Accumulation KG.zero
        begin "bench" KG.forty `shouldBe` JuggernautMethod "bench" Tens Accumulation KG.forty
        begin "asdfjkl;" LB.fortyFourPointOne `shouldBe` JuggernautMethod "asdfjkl;" Tens Accumulation LB.fortyFourPointOne

test_program_next :: Expectation
test_program_next = do
        next tensAccumulation fiveByFive `shouldBe` tensIntensification
        next tensRealization fiveByFive `shouldBe` JuggernautMethod exercise Tens Deload (Weight (-25) Pounds)
        next tensDeload fiveByFive `shouldBe` eightsAccumulation
        next eightsDeload fiveByFive `shouldBe` fivesAccumulation
        next fivesDeload fiveByFive `shouldBe` threesAccumulation
        next threesDeload fiveByFive `shouldBe` tensAccumulation
    where
        tensAccumulation = JuggernautMethod exercise Tens Accumulation zeroWeight
        exercise = "squat"
        zeroWeight = LB.zero
        fiveByFive = Workouts.fiveByFiveAtOneThirtyFive exercise
        tensIntensification = JuggernautMethod exercise Tens Intensification zeroWeight
        tensRealization = JuggernautMethod exercise Tens Realization zeroWeight
        tensDeload = JuggernautMethod exercise Tens Deload zeroWeight
        eightsAccumulation = JuggernautMethod exercise Eights Accumulation zeroWeight
        eightsDeload = JuggernautMethod exercise Eights Deload zeroWeight
        fivesAccumulation = JuggernautMethod exercise Fives Accumulation zeroWeight
        fivesDeload = JuggernautMethod exercise Fives Deload zeroWeight
        threesAccumulation = JuggernautMethod exercise Threes Accumulation zeroWeight
        threesDeload = JuggernautMethod exercise Threes Deload zeroWeight

test_program_as_workout :: Expectation
test_program_as_workout = do
        asWorkout tensRealization `shouldBe` tensRealizationWorkout
        asWorkout eightsRealization `shouldBe` eightsRealizationWorkout
        asWorkout eightsIntensification `shouldBe` eightsIntensificationWorkout
        asWorkout threesDeload `shouldBe` threesDeloadWorkout
    where
        tensRealization = JuggernautMethod exercise Tens Realization LB.oneThousand
        exercise = "bench press"
        tensRealizationWorkout = [
                calculateSet exercise 5 (Weight 500 Pounds) 1,
                calculateSet exercise 3 (Weight 600 Pounds) 1,
                calculateSet exercise 1 (Weight 700 Pounds) 1,
                calculateSet exercise 0 (Weight 750 Pounds) 1]
        eightsRealization = JuggernautMethod exercise Eights Realization LB.oneThousand
        eightsRealizationWorkout = [
                calculateSet exercise 5 (Weight 500 Pounds) 1,
                calculateSet exercise 3 (Weight 600 Pounds) 1,
                calculateSet exercise 2 (Weight 700 Pounds) 1,
                calculateSet exercise 1 (Weight 750 Pounds) 1,
                calculateSet exercise 0 (Weight 800 Pounds) 1]
        eightsIntensification = JuggernautMethod exercise Eights Intensification KG.oneThousand
        eightsIntensificationWorkout = [
                calculateSet exercise 3 (Weight 600 Kilograms) 1,
                calculateSet exercise 3 (Weight 675 Kilograms) 1,
                calculateSet exercise 8 (Weight 725 Kilograms) 1,
                calculateSet exercise 8 (Weight 725 Kilograms) 1,
                calculateSet exercise 0 (Weight 725 Kilograms) 1]
        threesDeload = JuggernautMethod exercise Threes Deload (Weight 11.4529 Kilograms)
        threesDeloadWorkout = [
                calculateSet exercise 5 (Weight 4.58116 Kilograms) 1,
                calculateSet exercise 5 (Weight 5.72645 Kilograms) 1,
                calculateSet exercise 5 (Weight 6.87174 Kilograms) 1]

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
        tensRealization = JuggernautMethod exercise Tens Realization LB.oneThousand
        exercise = "deadlift"
        fifteenReps = calculateSet exercise 15 tensRealizationWeight 1
        tensRealizationWeight = Weight 750 Pounds
        tensDeload = JuggernautMethod exercise Tens Deload
        twentyReps = calculateSet exercise 20 tensRealizationWeight 1
        eightsRealization = JuggernautMethod exercise Eights Realization LB.oneThousand
        thirteenReps = calculateSet exercise 13 (Weight 800 Pounds) 1
        eightsDeload = JuggernautMethod exercise Eights Deload
        threesRealization = JuggernautMethod exercise Threes Realization LB.oneThousand
        sevenReps = calculateSet exercise 7 (Weight 900 Pounds) 1
        threesDeload = JuggernautMethod exercise Threes Deload

test_limit_working_max :: Expectation
test_limit_working_max = do
        next tensRealization [fifteenReps] `shouldBe` tensDeload (Weight 408.5 Pounds)
        next fivesRealization [eightReps] `shouldBe` fivesDeload (Weight 969 Pounds)
    where
        tensRealization = JuggernautMethod exercise Tens Realization (Weight 400 Pounds)
        exercise = "overhead press"
        fifteenReps = calculateSet exercise 13 (Weight 300 Pounds) 1
        tensDeload = JuggernautMethod exercise Tens Deload
        fivesRealization = JuggernautMethod exercise Fives Realization LB.oneThousand
        eightReps = calculateSet exercise 6 (Weight 850 Pounds) 1
        fivesDeload = JuggernautMethod exercise Fives Deload

