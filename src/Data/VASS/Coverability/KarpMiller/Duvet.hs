module Data.VASS.Coverability.KarpMiller.Duvet where

import Data.VASS.Coverability.KarpMiller
import Duvet

karpMillerChecker :: CheckerInfo
karpMillerChecker = CheckerInfo
    { checker = karpMiller
    , longName = "karp-miller"
    , shortName = 'k'
    , description = "Use the standard Karp-Miller algorithm"
    }