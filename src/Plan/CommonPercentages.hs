module Plan.CommonPercentages (
        oneRepMaxVolumeScalar,
        forty,
        fifty,
        fiftyFive,
        sixty,
        sixtyTwoPointFive,
        sixtyFive,
        sixtySevenPointFive,
        seventy,
        seventyTwoPointFive,
        seventyFive,
        seventySevenPointFive,
        eighty,
        eightyTwoPointFive,
        eightyFive,
        ninety,
        ninetyFive
    ) where

import Data.Ratio

oneRepMaxVolumeScalar :: Rational
oneRepMaxVolumeScalar = 1 % 30

forty :: Rational
forty = 2 % 5

fifty :: Rational
fifty = 1 % 2

fiftyFive :: Rational
fiftyFive = 11 % 20

sixty :: Rational
sixty = 3 % 5

sixtyTwoPointFive :: Rational
sixtyTwoPointFive = 5 % 8

sixtyFive :: Rational
sixtyFive = 13 % 20

sixtySevenPointFive :: Rational
sixtySevenPointFive = 27 % 40

seventy :: Rational
seventy = 7 % 10

seventyTwoPointFive :: Rational
seventyTwoPointFive = 29 % 40

seventyFive :: Rational
seventyFive = 3 % 4

seventySevenPointFive :: Rational
seventySevenPointFive = 31 % 40

eighty :: Rational
eighty = 4 % 5

eightyTwoPointFive :: Rational
eightyTwoPointFive = 33 % 40

eightyFive :: Rational
eightyFive = 17 % 20

ninety :: Rational
ninety = 9 % 10

ninetyFive :: Rational
ninetyFive = 19 % 20

