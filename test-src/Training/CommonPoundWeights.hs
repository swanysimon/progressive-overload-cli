module Training.CommonPoundWeights (
        zero,
        fortyFourPointOne,
        fortyFive,
        ninety,
        oneThirtyFive,
        twoTwentyFive
    ) where

import Data.Ratio
import Training.Units
import Training.Weight

zero :: Weight
zero = Weight 0 Pounds

fortyFourPointOne :: Weight
fortyFourPointOne = Weight (441 % 10) Pounds

fortyFive :: Weight
fortyFive = Weight 45 Pounds

ninety :: Weight
ninety = Weight 90 Pounds

oneThirtyFive :: Weight
oneThirtyFive = Weight 135 Pounds

twoTwentyFive :: Weight
twoTwentyFive = Weight 225 Pounds

