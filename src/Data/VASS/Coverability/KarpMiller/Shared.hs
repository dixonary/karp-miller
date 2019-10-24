module Data.VASS.Coverability.KarpMiller.Shared where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.VASS
import Data.VASS.Coverability
import Data.VASS.Coverability.KarpMiller.ExtendedNaturals

import Data.Tree (Tree)
import qualified Data.Tree as Tree

import Data.Map (Map)
import qualified Data.Map as Map

--------------------------------------------------------------------------------
-- * Shared Types 

type KarpMillerTree = Tree ExtConf


--------------------------------------------------------------------------------
-- * Local helper functions

-- | Helper function defining containment over (almost) arbitrary functors.
contains :: (Foldable f, Ord a) => f a -> a -> Bool
obj `contains` item = any (item <=) obj

-- | Helper function for building default values out of things which have
-- a monoidal identity. (If the element isn't found in our map, we return empty.)
(!@) :: (Monoid a, Ord k) => Map k a -> k -> a
map !@ key = Map.findWithDefault mempty key map