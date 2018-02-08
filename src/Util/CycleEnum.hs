module Util.CycleEnum (
        CycleEnum,
        next,
        previous
    ) where

class (Bounded e, Enum e, Eq e) => CycleEnum e where
    next :: e -> e
    next enum
            | enum == maxBound = minBound
            | otherwise = succ enum

    previous :: e -> e
    previous enum
            | enum == minBound = maxBound
            | otherwise = pred enum

