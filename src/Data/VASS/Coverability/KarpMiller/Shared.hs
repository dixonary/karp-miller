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
-- * Shared Type Information

type KarpMillerTree = Tree ExtConf

contains :: (Foldable f, Ord a) => f a -> a -> Bool
obj `contains` item = any (item <=) obj

(!@) :: (Monoid a, Ord k) => Map k a -> k -> a
map !@ key = Map.findWithDefault mempty key map