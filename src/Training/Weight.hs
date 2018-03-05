module Training.Weight (
        MinimumWeightJump,
        Units(..),
        Weight(..),
        WorkingMax,
        addWeight,
        addWeights ,
        convertWeightUnits,
        multiplyWeight
    ) where

import Data.Ratio

type MinimumWeightJump = Weight
type WorkingMax = Weight

data Units = Kilograms | Pounds
    deriving (Eq, Read, Show)

data Weight = Weight Rational Units
    deriving (Read, Show)

instance Eq Weight where
    (==) (Weight x units) otherWeight = x == y && units == newUnits
        where
            (Weight y newUnits) = convertWeightUnits otherWeight units

instance Ord Weight where
    compare (Weight x units) otherWeight = compare x y
        where
            (Weight y _) = convertWeightUnits otherWeight units

addWeight :: (Real a) => Weight -> a -> Weight
addWeight (Weight x units) weightToAdd = Weight newWeight units
    where
        rationalWeightToAdd = toRational weightToAdd
        newWeight = x + rationalWeightToAdd

addWeights :: Weight -> Weight -> Weight
addWeights weight@(Weight x units) otherWeight@(Weight y otherUnits)
    | units == otherUnits = Weight (x + y) units
    | otherwise = addWeights weight convertedWeight
        where
            convertedWeight = convertWeightUnits otherWeight units

convertWeightUnits :: Weight -> Units -> Weight
convertWeightUnits weight@(Weight x oldUnits) newUnits
    | newUnits == oldUnits = weight
    | newUnits == Pounds = Weight newPoundWeight Pounds
    | newUnits == Kilograms = Weight newKiloWeight Kilograms
    | otherwise = error $ "Unknown unit provided in weight: " ++ show weight
        where
            newPoundWeight = x * kiloToPoundRatio
            kiloToPoundRatio = 441 % 200 -- 20 kilograms is exactly 44.1 pounds for our purposes
            newKiloWeight = x * (1 / kiloToPoundRatio)

multiplyWeight :: (Real a) => Weight -> a -> Weight
multiplyWeight (Weight x units) scalar = Weight newWeight units
    where
        newWeight = x * rationalScalar
        rationalScalar = toRational scalar

