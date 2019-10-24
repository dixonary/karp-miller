{-| Integration with the duvet coverability checker, due for public release
    in early 2020. For more information please see the duvet package.
-}
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