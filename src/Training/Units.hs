module Training.Units (
        Units(Kilograms, Pounds)
    ) where

data Units = Kilograms | Pounds
    deriving (Eq, Read, Show)

