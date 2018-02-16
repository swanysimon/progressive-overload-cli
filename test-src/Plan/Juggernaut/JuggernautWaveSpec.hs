module Plan.Juggernaut.JuggernautWaveSpec (
        main, -- only here so module can be run from ghci for debugging
        spec
    ) where

import qualified Util.CycleEnum as Cycle
import Plan.Juggernaut.JuggernautWave
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
        describe "instance of CycleEnum" cycleEnumInstanceTests
        describe "function expectedRepetitions" expectedRepetitionsTests

cycleEnumInstanceTests :: SpecWith ()
cycleEnumInstanceTests = do
        it "tests JuggernautWave instance of CycleEnum.next" test_cycle_enum_next
        it "tests JuggernautWave instance of CycleEnum.previous" test_cycle_enum_previous

test_cycle_enum_next :: Expectation
test_cycle_enum_next = do
        Cycle.next Tens `shouldBe` Eights
        Cycle.next Eights `shouldBe` Fives
        Cycle.next Fives `shouldBe` Threes
        Cycle.next Threes `shouldBe` Tens

test_cycle_enum_previous :: Expectation
test_cycle_enum_previous = do
        Cycle.previous Tens `shouldBe` Threes
        Cycle.previous Eights `shouldBe` Tens
        Cycle.previous Fives `shouldBe` Eights
        Cycle.previous Threes `shouldBe` Fives

expectedRepetitionsTests :: SpecWith ()
expectedRepetitionsTests = do
        it "tests wave name corresponds to expected repetitions for wave" test_expected_repetitions

test_expected_repetitions :: Expectation
test_expected_repetitions = do
        expectedRepetitions Tens `shouldBe` 10
        expectedRepetitions Eights `shouldBe` 8
        expectedRepetitions Fives `shouldBe` 5
        expectedRepetitions Threes `shouldBe` 3

