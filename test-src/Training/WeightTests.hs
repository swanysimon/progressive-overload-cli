module Training.WeightTests (
        main, -- only here so module can be run from ghci for debugging
        spec
    ) where

import Test.Hspec
import Training.Units
import Training.Weight
import Training.WeightTestsHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "addWeight" addScalarToWeightTests
        describe "addWeights" addWeightToWeightTests
        describe "convertWeightUnits" convertWeightUnitsTests
        describe "multiplyWeight" multiplyWeightByScalarTests

addScalarToWeightTests :: SpecWith ()
addScalarToWeightTests = do
        it "tests 0 kilograms plus 20 is 20 kilograms" test_0_kilograms_plus_20
        it "tests 10 random kilogram additions" test_random_kilogram_additions
        it "tests 10 random pound additions" test_random_pound_additions
        -- if any of the randomized tests ever fails, add the specific test case below this line

test_0_kilograms_plus_20 :: Expectation
test_0_kilograms_plus_20 = checkWeightsEqual zeroPlusTwentyKilograms twentyKg
    where
        zeroPlusTwentyKilograms = addWeight zeroKg (20 :: Double)

test_random_kilogram_additions :: Expectation
test_random_kilogram_additions = checkTenRandomAdditions Kilograms

test_random_pound_additions :: Expectation
test_random_pound_additions = checkTenRandomAdditions Pounds

addWeightToWeightTests :: SpecWith ()
addWeightToWeightTests = do
        it "tests 0 kilograms plus 0 pounds is 0 kilograms" test_0_kilograms_plus_0_pounds
        it "tests 20 kilograms plus 44.1 pounds is 40 kilograms" test_20_kilograms_plus_44_1_pounds
        it "tests 10 random kilograms plus kilograms operations" test_random_kilograms_plus_kilograms_operations
        it "tests 10 random pounds plus pounds operations" test_random_pounds_plus_pounds_operations
        -- if any of the randomized tests ever fails, add the specific test case below this line

test_0_kilograms_plus_0_pounds :: Expectation
test_0_kilograms_plus_0_pounds = do
        checkWeightsEqual zeroWeightsAdded zeroKg
        w `shouldBe` 0
        units `shouldBe` Kilograms
    where
        zeroWeightsAdded@(Weight w units) = addWeights zeroKg zeroPounds

test_20_kilograms_plus_44_1_pounds :: Expectation
test_20_kilograms_plus_44_1_pounds = do
        checkWeightsEqual addedWeights fortyKg
        w `shouldBe` 40
        units `shouldBe` Kilograms
    where
        addedWeights@(Weight w units) = addWeights twentyKg fortyFourOnePounds

test_random_kilograms_plus_kilograms_operations :: Expectation
test_random_kilograms_plus_kilograms_operations = checkTenRandomSameUnitAdditions Kilograms

test_random_pounds_plus_pounds_operations :: Expectation
test_random_pounds_plus_pounds_operations = checkTenRandomSameUnitAdditions Pounds

convertWeightUnitsTests :: SpecWith ()
convertWeightUnitsTests = do
        it "tests 0 kilograms is 0 pounds" test_0_kilograms_is_0_pounds
        it "tests 20 kilograms is 44.1 pounds" test_20_kilograms_is_44_1_pounds
        it "tests 10 random kilogram conversions" test_random_kilogram_conversions
        it "tests 10 random pound conversions" test_random_pound_conversions
        -- if any of the randomized tests ever fails, add the specific test case below this line

test_0_kilograms_is_0_pounds :: Expectation
test_0_kilograms_is_0_pounds = checkWeightsEqual zeroKg zeroPounds

test_20_kilograms_is_44_1_pounds :: Expectation
test_20_kilograms_is_44_1_pounds = checkWeightsEqual twentyKg fortyFourOnePounds

test_random_kilogram_conversions :: Expectation
test_random_kilogram_conversions = checkTenRandomConversions Kilograms Pounds

test_random_pound_conversions :: Expectation
test_random_pound_conversions = checkTenRandomConversions Pounds Kilograms

multiplyWeightByScalarTests :: SpecWith ()
multiplyWeightByScalarTests = do
        it "tests 0 kilograms times any scalar is 0 kilograms" test_0_kilogram_multiplication
        it "tests 20 kilograms times 2 is 40 kilograms" test_20_kilograms_times_2
        it "tests 10 random kilograms products" test_random_kilogram_products
        -- if any of the randomized tests ever fails, add the specific test case below this line

test_0_kilogram_multiplication :: Expectation
test_0_kilogram_multiplication = do
        randomNum <- getRandomRational
        let weightProduct = multiplyWeight zeroKg randomNum
        checkWeightsEqual zeroKg weightProduct

test_20_kilograms_times_2 :: Expectation
test_20_kilograms_times_2 = checkWeightsEqual fortyKg weightProduct
    where
        weightProduct = multiplyWeight twentyKg (2 :: Double)

test_random_kilogram_products :: Expectation
test_random_kilogram_products = checkTenRandomProducts Kilograms

