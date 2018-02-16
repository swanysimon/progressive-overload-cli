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
import Training.Units
import Training.Weight

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
        describe "instance of Program" programInstanceTests

programInstanceTests :: SpecWith ()
programInstanceTests = do
        it "tests JuggernautMethod instance of Program.begin" test_program_begin
        it "tests JuggernautMethod instance of Program.next" test_program_next
        -- it "tests JuggernautMethod instance of Program.asWorkout" test_program_as_workout

test_program_begin :: Expectation
test_program_begin = do
        begin "squat" KG.zero `shouldBe` JuggernautMethod "squat" Tens Accumulation KG.zero
        begin "bench" KG.forty `shouldBe` JuggernautMethod "bench" Tens Accumulation KG.forty
        begin "asdfjkl;" LB.fortyFourPointOne `shouldBe` JuggernautMethod "asdfjkl;" Tens Accumulation LB.fortyFourPointOne

test_program_next :: Expectation
test_program_next = do
        next (JuggernautMethod "squat" Tens Accumulation LB.zero) (Workouts.fiveByFiveAtOneThirtyFive "squat")
                `shouldBe`
                JuggernautMethod "squat" Tens Intensification LB.zero
        next (JuggernautMethod "squat" Tens Realization LB.zero) (Workouts.fiveByFiveAtOneThirtyFive "squat")
                `shouldBe`
                JuggernautMethod "squat" Tens Deload (Weight (-25) Pounds)
        next (JuggernautMethod "squat" Tens Deload LB.zero) (Workouts.fiveByFiveAtOneThirtyFive "squat")
                `shouldBe`
                JuggernautMethod "squat" Eights Accumulation LB.zero
        next (JuggernautMethod "squat" Eights Accumulation LB.zero) (Workouts.fiveByFiveAtOneThirtyFive "squat")
                `shouldBe`
                JuggernautMethod "squat" Eights Intensification LB.zero
        next (JuggernautMethod "squat" Eights Deload LB.zero) (Workouts.fiveByFiveAtOneThirtyFive "squat")
                `shouldBe`
                JuggernautMethod "squat" Fives Accumulation LB.zero
        next (JuggernautMethod "squat" Fives Deload LB.zero) (Workouts.fiveByFiveAtOneThirtyFive "squat")
                `shouldBe`
                JuggernautMethod "squat" Threes Accumulation LB.zero
        next (JuggernautMethod "squat" Threes Deload LB.zero) (Workouts.fiveByFiveAtOneThirtyFive "squat")
                `shouldBe`
                JuggernautMethod "squat" Tens Accumulation LB.zero

