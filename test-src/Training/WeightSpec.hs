{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Training.WeightSpec (
        main, -- only here so module can be run from ghci for debugging
        spec
    ) where

import qualified Plan.CommonPercentages as Percentage
import qualified Training.CommonKilogramWeights as KG
import qualified Training.CommonPoundWeights as LB
import Data.Ratio
import Test.Hspec
import Training.Units
import Training.Weight

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
        describe "instance of Ord" orderingInstanceTests
        describe "instance of Eq" equalityInstanceTests
        describe "function addWeight" addWeightTests
        describe "function addWeights" addWeightsTests
        describe "function convertWeightUnits" convertWeightUnitsTests
        describe "function multiplyWeight" multiplyWeightTests

orderingInstanceTests :: SpecWith ()
orderingInstanceTests = do
        it "tests Weight instance of Ord.(<)" test_weight_ord_less_than

test_weight_ord_less_than :: Expectation
test_weight_ord_less_than = do
        KG.zero `shouldBeLessThan` KG.twenty
        KG.zero `shouldBeLessThan` LB.fortyFourPointOne
        LB.fortyFourPointOne `shouldBeLessThan` KG.forty
        KG.zero `shouldNotBeLessThan` KG.zero

shouldBeLessThan :: Weight -> Weight -> Expectation
shouldBeLessThan weight otherWeight = weight `shouldSatisfy` (otherWeight >)

shouldNotBeLessThan :: Weight -> Weight -> Expectation
shouldNotBeLessThan weight otherWeight = do
        weight `shouldNotSatisfy` (otherWeight >)
        weight `shouldSatisfy` (otherWeight <=)

equalityInstanceTests :: SpecWith ()
equalityInstanceTests = do
        it "tests weight is equal to itself" test_weight_equal_to_itself
        it "tests weight equality with conversions" test_weight_equal_with_conversion
        it "test weight inequality" test_weight_not_equal

test_weight_equal_to_itself :: Expectation
test_weight_equal_to_itself = do
        KG.zero `shouldBeExactWeight` KG.zero
        KG.twenty `shouldBeExactWeight` KG.twenty
        LB.zero `shouldBeExactWeight` LB.zero
        LB.fortyFourPointOne `shouldBeExactWeight` LB.fortyFourPointOne
        Weight (-1290.14904) Pounds `shouldBeExactWeight` Weight (-1290.14904) Pounds

shouldBeExactWeight :: Weight -> Weight -> Expectation
shouldBeExactWeight weight@(Weight x units) otherWeight@(Weight y otherUnits) = do
        weight `shouldBe` otherWeight
        (x, units) `shouldBe` (y, otherUnits)

test_weight_equal_with_conversion :: Expectation
test_weight_equal_with_conversion  = do
        KG.zero `shouldBeSameWeightDifferentUnits` LB.zero
        KG.twenty `shouldBeSameWeightDifferentUnits` LB.fortyFourPointOne
        Weight 1000 Kilograms `shouldBeSameWeightDifferentUnits` Weight 2205 Pounds

shouldBeSameWeightDifferentUnits :: Weight -> Weight -> Expectation
shouldBeSameWeightDifferentUnits weight@(Weight _ units) otherWeight@(Weight _ otherUnits) = do
        weight `shouldBe` otherWeight
        units `shouldNotBe` otherUnits

test_weight_not_equal :: Expectation
test_weight_not_equal = do
        KG.zero `shouldNotBe` KG.twenty
        LB.fortyFourPointOne `shouldNotBe` KG.forty
        Weight 1280395 Pounds `shouldNotBe` Weight (-19) Kilograms

addWeightTests :: SpecWith ()
addWeightTests = do
        it "tests adding to weight" test_add_weight

test_add_weight :: Expectation
test_add_weight = do
        addWeight KG.zero 20 `shouldBeExactWeight` KG.twenty
        addWeight LB.zero 135 `shouldBeExactWeight` LB.oneThirtyFive
        addWeight KG.zero 40 `shouldBeExactWeight` KG.forty
        addWeight KG.forty 90 `shouldBeExactWeight` Weight 130 Kilograms
        addWeight KG.zero (-20.1253) `shouldBeExactWeight` Weight (toRational (-20.1253)) Kilograms

addWeightsTests :: SpecWith ()
addWeightsTests = do
        it "tests same unit weight additions" test_add_weights
        it "tests different unit weight additions" test_add_weights_with_conversion

test_add_weights :: Expectation
test_add_weights = do
        addWeights KG.zero KG.zero `shouldBeExactWeight` KG.zero
        addWeights KG.zero KG.twenty `shouldBeExactWeight` KG.twenty
        addWeights KG.twenty KG.zero `shouldBeExactWeight` KG.twenty
        addWeights KG.twenty KG.twenty `shouldBeExactWeight` KG.forty
        addWeights LB.zero LB.fortyFourPointOne `shouldBeExactWeight` LB.fortyFourPointOne
        addWeights LB.twoTwentyFive LB.ninety `shouldBeExactWeight` Weight 315 Pounds
        addWeights LB.oneThirtyFive (Weight (-90) Pounds) `shouldBeExactWeight` LB.fortyFive
        addWeights (Weight 1204 Kilograms) (Weight (1 % 100) Pounds) `shouldBeExactWeight` Weight (530966 % 441) Kilograms

test_add_weights_with_conversion :: Expectation
test_add_weights_with_conversion = do
        addWeights KG.zero LB.zero `shouldBeExactWeight` KG.zero
        addWeights KG.zero LB.fortyFourPointOne `shouldBeExactWeight` KG.twenty
        addWeights LB.fortyFourPointOne KG.zero `shouldBeSameWeightDifferentUnits` KG.twenty
        addWeights (Weight 405 Pounds) KG.forty `shouldBeExactWeight` Weight (2466 % 5) Pounds

convertWeightUnitsTests :: SpecWith ()
convertWeightUnitsTests = do
        it "tests weight conversion" test_convert_weight_units

test_convert_weight_units :: Expectation
test_convert_weight_units = do
        convertWeightUnits KG.zero Pounds `shouldBeExactWeight` LB.zero
        convertWeightUnits KG.zero Pounds `shouldBeSameWeightDifferentUnits` KG.zero
        convertWeightUnits LB.zero Kilograms `shouldBeExactWeight` KG.zero
        convertWeightUnits KG.twenty Pounds `shouldBeExactWeight` LB.fortyFourPointOne
        convertWeightUnits KG.forty Kilograms `shouldBeExactWeight` KG.forty
        convertWeightUnits LB.fortyFourPointOne Pounds `shouldBeExactWeight` LB.fortyFourPointOne
        convertWeightUnits (Weight 30941.5 Pounds) Pounds `shouldBeExactWeight` Weight 30941.5 Pounds

multiplyWeightTests :: SpecWith ()
multiplyWeightTests = do
        it "tests multiplying by a weight" test_multiply_weights

test_multiply_weights :: Expectation
test_multiply_weights = do
        multiplyWeight KG.zero 140289 `shouldBeExactWeight` KG.zero
        multiplyWeight KG.forty Percentage.fifty `shouldBeExactWeight` KG.twenty
        multiplyWeight LB.fortyFourPointOne 0 `shouldBeExactWeight` LB.zero

