module Training.CommonPoundWeights (
        zero,
        fortyFourPointOne
    ) where

import Data.Ratio
import Training.Units
import Training.Weight

zero :: Weight
zero = Weight 0 Pounds

fortyFourPointOne :: Weight
fortyFourPointOne = Weight (441 % 10) Pounds

