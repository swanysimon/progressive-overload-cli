module Training.WeightTests (
        main, -- only here so module can be run from ghci for debugging
        spec
    ) where

import Data.Ratio
import System.Random
import Test.Hspec
import Training.Units
import Training.Weight

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "convertWeightUnits" testConvertUnits
        describe "addWeight" testAddWeight

testConvertUnits :: SpecWith ()
testConvertUnits = do
        it "tests 0 kilograms is 0 pounds" test_0_kilograms_is_0_pounds
        it "tests 20 kilograms is 44.1 pounds" test_20_kilograms_is_44_1_pounds
        it "tests 10 random kilogram conversions" test_random_kilogram_conversions
        it "tests 10 random pound conversions" test_random_pound_conversions
        -- if any of the random number tests ever fails, add it's specific test case below this line

test_0_kilograms_is_0_pounds :: Expectation
test_0_kilograms_is_0_pounds = checkWeightsEqual zeroKg zeroPounds

zeroKg :: Weight
zeroKg = Weight 0 Kilograms

zeroPounds :: Weight
zeroPounds = Weight 0 Pounds

checkWeightsEqual :: Weight -> Weight -> Expectation
checkWeightsEqual weight@(Weight _ units) otherWeight@(Weight _ otherUnits) = do
        weight `shouldBeSymmetric` otherWeight
        convertedWeight `shouldBeSymmetric` otherWeight
        convertedWeight `shouldBeSymmetric` convertedOtherWeight
    where
        convertedWeight = convertWeightUnits weight otherUnits
        convertedOtherWeight = convertWeightUnits otherWeight units

shouldBeSymmetric :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
shouldBeSymmetric x y = do
        x `shouldBe` y
        y `shouldBe` x

test_20_kilograms_is_44_1_pounds :: Expectation
test_20_kilograms_is_44_1_pounds = checkWeightsEqual twentyKg fortyFourOnePounds
    where
        twentyKg = Weight 20 Kilograms
        fortyFourOnePounds = Weight 44.1 Pounds

test_random_kilogram_conversions :: Expectation
test_random_kilogram_conversions = checkTenRandomConversions (\w -> Weight w Kilograms) Pounds

checkTenRandomConversions :: (Rational -> Weight) -> Units -> Expectation
checkTenRandomConversions newWeightFunction newUnits = do
        randomWeights <- getNRandomWeights 10 newWeightFunction
        mapM_ checkConvertedWeight randomWeights
    where
        checkConvertedWeight weight = checkWeightsEqual weight (convertedWeight weight)
        convertedWeight weight = convertWeightUnits weight newUnits

getNRandomWeights :: Int -> (Rational -> Weight) -> IO [Weight]
getNRandomWeights n newWeightFunction = fmap toRandomWeights newStdGen
    where
        toRandomWeights = take n . map randomToWeight . randomRs randomRange
        randomToWeight = newWeightFunction . toRational

-- need the number to be positive and the upper bound is something unreasonable to lift regardless of unit
randomRange :: (Double, Double)
randomRange = (0, 5000)

test_random_pound_conversions :: Expectation
test_random_pound_conversions = checkTenRandomConversions (\w -> Weight w Pounds) Kilograms

testAddWeight :: SpecWith ()
testAddWeight = do
        it "tests 0 kilograms plus 10 is 10 kilograms" test_0_kilograms_plus_10
        it "tests 10 random kilogram additions" test_random_kilogram_additions
        it "tests 10 random pound additions" test_random_pound_additions
        -- if any of the random number tests ever fails, add it's specific test case below this line

test_0_kilograms_plus_10 :: Expectation
test_0_kilograms_plus_10 = checkWeightsEqual zeroPlusTenKilograms tenKilograms
    where
        zeroPlusTenKilograms = addWeight zeroKg ten
        ten = (10 % 1)
        tenKilograms = Weight ten Kilograms

test_random_kilogram_additions :: Expectation
test_random_kilogram_additions = checkTenRandomAdditions (\w -> Weight w Kilograms)

checkTenRandomAdditions :: (Rational -> Weight) -> Expectation
checkTenRandomAdditions newWeightFunction = do
        randomWeights <- getNRandomWeights 10 newWeightFunction
        mapM_ checkAddedWeight randomWeights

checkAddedWeight :: Weight -> Expectation
checkAddedWeight weight@(Weight w units) = do
        randomNumToAdd <- fmap toRandomRational newStdGen
        let addedWeight = addWeight weight randomNumToAdd
        let directlyAddedWeight = Weight (w + randomNumToAdd) units
        checkWeightsEqual addedWeight directlyAddedWeight
    where
        toRandomRational = toRational . fst . randomR randomRange

test_random_pound_additions :: Expectation
test_random_pound_additions = checkTenRandomAdditions (\w -> Weight w Pounds)

