module Plan.Juggernaut.JuggernautPhaseSpec (
        main, -- only here so module can be run from ghci for debugging
        spec
    ) where

import qualified Util.CycleEnum as Cycle
import Plan.Juggernaut.JuggernautPhase
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
        describe "instance of CycleEnum" cycleEnumInstanceTests

cycleEnumInstanceTests :: SpecWith ()
cycleEnumInstanceTests = do
        it "tests JuggernautPhase instance of CycleEnum.next" test_cycle_enum_next
        it "tests JuggernautPhase instance of CycleEnum.previous" test_cycle_enum_previous

test_cycle_enum_next :: Expectation
test_cycle_enum_next = do
        Cycle.next Accumulation `shouldBe` Intensification
        Cycle.next Intensification `shouldBe` Realization
        Cycle.next Realization `shouldBe` Deload
        Cycle.next Deload `shouldBe` Accumulation

test_cycle_enum_previous :: Expectation
test_cycle_enum_previous = do
        Cycle.previous Accumulation `shouldBe` Deload
        Cycle.previous Intensification `shouldBe` Accumulation
        Cycle.previous Realization `shouldBe` Intensification
        Cycle.previous Deload `shouldBe` Realization

