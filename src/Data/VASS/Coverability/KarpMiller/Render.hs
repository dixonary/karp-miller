{-| Tools for drawing the trees constructed by the Karp-Miller algorithm.

    This will output SVG diagrams using the Diagrams package.

    Note: You may find that, on large outputs, this takes a very, very long time!
-}
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Data.VASS.Coverability.KarpMiller.Render where

import Data.VASS
import Data.VASS.Coverability

import Data.VASS.Coverability.KarpMiller.ExtendedNaturals
import Data.VASS.Coverability.KarpMiller.Shared
import Data.VASS.Coverability.KarpMiller (karpMillerTree)

import Diagrams.Prelude hiding (render)
import Data.Tree (Tree(..))
import qualified Data.Vector as Vector
import Diagrams.Backend.SVG
import Data.Function ((&))
import Graphics.SVGFonts

import Diagrams.TwoD.Layout.Tree


-- | Given a VAS and a filepath, render the Karp-Miller tree as an SVG 
-- and save it to that file.
renderSpec :: CovProblem -> FilePath -> IO ()
renderSpec (CovProblem v i t) path = render path $ karpMillerTree i v

-- | Given a file path, output the generated Karp-Miller tree as an SVG.
render :: FilePath -> KarpMillerTree -> IO ()
render path kmtree = renderSVG path size (scale sf diagram)
    where hsep = let Node a _ = kmtree in fromIntegral (dim a + 4)
          diagram = renderTree
            (text)
            {-
            (arrowBetween' (with & headLength .~ normalized 0.05
                                 & headGap .~ normalized 0.1
                                 & tailGap .~ normalized 0.1))
            -}
            (arrowBetween' (with & shaftStyle %~ lw (local 0.1)
                                 & headLength .~ local 1
                                 & headGap .~ local 1
                                 & tailGap .~ local 1))
            (symmLayout' (with & slHSep .~ hsep & slVSep .~ 10) (toCTree kmtree))
            # centerXY # pad 1.1

          -- text' t = stroke (textSVG t 1)
          size :: SizeSpec V2 Double
          --size = absolute
          size = dims $ r2 (800,800)

          sf :: Double
          --sf = 4
          sf = 1

{-| Helper function for rendering the parts of the Karp-Miller trees into strings.

    Nodes with no children (ie dead ends) are marked with a box.
-}
toCTree :: KarpMillerTree -> Tree String
toCTree (Node a []) = Node (show a++" ■") []
toCTree (Node a cs) = Node (show a) $ fmap toCTree cs