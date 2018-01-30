module Training.Weight (
        Weight,
        addWeight,
        addWeights ,
        convertWeightUnits,
        multiplyWeight
    ) where

import Training.Units

data Weight = Weight Rational Units

instance Eq Weight where
    (==) (Weight x units) otherWeight = x == y
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

multiplyWeight :: (Real a) => Weight -> a -> Weight
multiplyWeight (Weight x units) scalar = Weight newWeight units
    where
        rationalScalar = toRational scalar
        newWeight = x * rationalScalar

convertWeightUnits :: Weight -> Units -> Weight
convertWeightUnits weight@(Weight x oldUnits) newUnits
    | newUnits == oldUnits = weight
    | newUnits == Pounds = Weight newPoundWeight Pounds
    | newUnits == Kilograms = Weight newKiloWeight Kilograms
        where
            kiloToPoundRatio = toRational 2.205
            newPoundWeight = x * kiloToPoundRatio
            newKiloWeight = x * (1 / kiloToPoundRatio)

