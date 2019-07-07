module Data.VASS.Coverability.KarpMiller (karpMiller, karpMillerTree) where

-- | Datatypes
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Graph as Tree
import Data.Graph (Tree(..))
import qualified Data.List as List
import Data.Maybe

import Text.Pretty.Simple

import Data.VASS.Coverability.KarpMiller.Shared
import Data.VASS.Coverability.KarpMiller.ExtendedNaturals

import Data.VASS.Coverability
import Data.VASS


karpMiller :: CovChecker
karpMiller (CovProblem vass initial target) = 
    let
        tree = karpMillerTree initial vass
    in return $ if tree `contains` extend target
            then Unsafe
            else Safe


{- | Construct the Karp-Miller Tree which represents the coverability set.
    This is an implementation of the standard algorithm, and does not include
    any accelerations. It forms a useful baseline to confirm that accelerations
    are giving the correct results.

    The original KM definition performed a breadth-first construction; as there is
    no pruning, the tree is always maximal and therefore the order of construction
    is not relevant. Depth-first search is more natural in a recursive format.
-}
karpMillerTree :: Conf -> VASS -> KarpMillerTree
karpMillerTree initial VASS{..} = let

    -- | All VASS states which can be reached by one transition from our
    -- current configuration.
    reachableFrom :: ExtConf -> [ExtConf]
    reachableFrom conf@(Configuration state vec) = 
        [ conf |> trans
        | trans <- Vector.toList $ transitions !@ state
        , trans `activeFrom` conf
        ]

    -- | Our acceleration step
    -- we can jump to omega in any places which strictly increase.
    addOmegas :: ExtConf -> ExtConf -> ExtConf
    addOmegas (Configuration s vec) (Configuration s' ancestor)
        | s /= s'          = Configuration s vec
        | ancestor <= vec  = Configuration s (Vector.zipWith makeOmega vec ancestor)
        | otherwise        = Configuration s vec
        where makeOmega v a 
                | v > a    = Omega
                | otherwise = v

    -- | Recursive depth-first construction of the KM tree.
    treeRec :: [ExtConf] -> ExtConf -> Maybe KarpMillerTree
    treeRec ancestors current@(Configuration state vec) = let

        current'   = List.foldl' addOmegas current ancestors
        reachables = reachableFrom current'

        -- If the node has previously been seen, we will not build it
        in 
            if ancestors `contains` current then Nothing
            else
            Just $ Node current' 
                $ catMaybes 
                $ treeRec (current':ancestors) <$> reachables

    in fromJust $ treeRec [] (extend initial)