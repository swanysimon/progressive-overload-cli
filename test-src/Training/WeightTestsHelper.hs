module Training.WeightTestsHelper (
        checkTenRandomAdditions,
        checkTenRandomConversions,
        checkTenRandomProducts,
        checkTenRandomSameUnitAdditions,
        checkWeightsEqual,
        getRandomRational
    ) where

import System.Random
import Test.Hspec
import Training.Units
import Training.Weight

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

checkTenRandomConversions :: Units -> Units -> Expectation
checkTenRandomConversions units newUnits = do
        randomWeights <- getTenRandomWeights units
        mapM_ checkConvertedWeight randomWeights
    where
        checkConvertedWeight weight = checkWeightsEqual weight (convertedWeight weight)
        convertedWeight weight = convertWeightUnits weight newUnits

rationalToWeightFunction :: Units -> (Rational -> Weight)
rationalToWeightFunction units = (\w -> Weight w units)

getTenRandomWeights :: Units -> IO [Weight]
getTenRandomWeights units = fmap toRandomWeights newStdGen
    where
        toRandomWeights = take 10 . map randomToWeight . randomRs randomRange
        randomToWeight = rationalToWeightFunction units . toRational

-- need the number to be positive and the upper bound is something unreasonable to lift regardless of unit
randomRange :: (Double, Double)
randomRange = (0, 5000)

checkTenRandomAdditions :: Units -> Expectation
checkTenRandomAdditions units = do
        randomWeights <- getTenRandomWeights units
        mapM_ getAndCheckRandomAddedWeight randomWeights

getAndCheckRandomAddedWeight :: Weight -> Expectation
getAndCheckRandomAddedWeight weight@(Weight w units) = do
        randomNumToAdd <- getRandomRational
        let addedWeight = addWeight weight randomNumToAdd
        let directlyAddedWeight = Weight (w + randomNumToAdd) units
        checkWeightsEqual addedWeight directlyAddedWeight

getRandomRational :: IO Rational
getRandomRational = fmap toRandomRational newStdGen
    where
        toRandomRational = toRational . fst . randomR randomRange

checkTenRandomSameUnitAdditions :: Units -> Expectation
checkTenRandomSameUnitAdditions units = do
        randomWeights <- getTenRandomWeights units
        mapM_ getAndCheckSameUnitAddedRandomizedWeights randomWeights

getAndCheckSameUnitAddedRandomizedWeights :: Weight -> Expectation
getAndCheckSameUnitAddedRandomizedWeights weight@(Weight w units) = do
        randomWeight@(Weight x _) <- getRandomWeight units
        let addedWeights = addWeights weight randomWeight
        let directlyAddedWeights = Weight (w + x) units
        checkWeightsEqual addedWeights directlyAddedWeights

getRandomWeight :: Units -> IO Weight
getRandomWeight units = fmap (rationalToWeightFunction units) getRandomRational

checkTenRandomProducts :: Units -> Expectation
checkTenRandomProducts units = do
        randomWeights <- getTenRandomWeights units
        mapM_ getAndCheckRandomMultipliedWeight randomWeights

getAndCheckRandomMultipliedWeight :: Weight -> Expectation
getAndCheckRandomMultipliedWeight weight@(Weight w units) = do
        randomNum <- getRandomRational
        let multipliedWeight = multiplyWeight weight randomNum
        let directlyMultipliedWeight = Weight (w * randomNum) units
        checkWeightsEqual multipliedWeight directlyMultipliedWeight

