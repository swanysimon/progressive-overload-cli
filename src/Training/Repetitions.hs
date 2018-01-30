module Training.Repetitions (
        Repetitions
    ) where

newtype Repetitions = Repetitions Integer
    deriving (Eq, Ord)

instance Num Repetitions where
    (+) (Repetitions x) (Repetitions y) = Repetitions (x + y)

    (*) (Repetitions x) (Repetitions y) = Repetitions (x * y)

    (-) (Repetitions x) (Repetitions y) = Repetitions (x - y)

    abs (Repetitions x) = Repetitions (abs x)

    signum (Repetitions x) = Repetitions (signum x)

    fromInteger x = Repetitions x

    negate (Repetitions x) = Repetitions (negate x)

instance Real Repetitions where
    toRational (Repetitions x) = toRational x

