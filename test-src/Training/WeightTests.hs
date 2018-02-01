module Training.WeightTests (
        main, -- only here so module can be run from ghci for debugging
        spec
    ) where

import System.Random
import Test.Hspec
import Training.Units
import Training.Weight

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "convertWeightUnits" testConvertUnits

testConvertUnits :: SpecWith ()
testConvertUnits = do
        it "tests 0 kilograms is 0 pounds" test_0_kilograms_is_0_pounds
        it "tests 20 kilograms is 44.1 pounds" test_20_kilograms_is_44_1_pounds
        it "tests random kilogram conversions" test_random_kilogram_conversions
        it "tests random pound conversions" test_random_pound_conversions
        -- if any of the random number tests ever fails, add it's specific test case below this line

test_0_kilograms_is_0_pounds :: Expectation
test_0_kilograms_is_0_pounds = checkWeightsEqual zeroKg zeroPounds
    where
        zeroKg = Weight 0 Kilograms
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
test_random_kilogram_conversions = checkTenRandomConversions (\x -> Weight x Kilograms) Pounds

checkTenRandomConversions :: (Rational -> Weight) -> Units -> Expectation
checkTenRandomConversions newWeightFunction newUnits = do
        -- limit to positive numbers less than some unreasonable amount of weight in either unit
        randomWeights <- fmap (take 10 . map randomToWeight . randomRs randomRange) newStdGen
        mapM_ checkConvertedWeight randomWeights
    where
        randomRange = (0, 5000) :: (Double, Double)
        randomToWeight = newWeightFunction . toRational
        checkConvertedWeight weight = checkWeightsEqual weight (convertedWeight weight)
        convertedWeight weight = convertWeightUnits weight newUnits

test_random_pound_conversions :: Expectation
test_random_pound_conversions = checkTenRandomConversions (\x -> Weight x Pounds) Kilograms

