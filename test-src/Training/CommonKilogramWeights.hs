module Training.CommonKilogramWeights (
        zero,
        twenty,
        forty,
        oneThousand
    ) where

import Training.Units
import Training.Weight

zero :: Weight
zero = Weight 0 Kilograms

twenty :: Weight
twenty = Weight 20 Kilograms

forty :: Weight
forty = Weight 40 Kilograms

oneThousand :: Weight
oneThousand = Weight 1000 Kilograms

