{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- Borrowed from the phybin <http://hackage.haskell.org/package/phybin> package
module Bio.Phylogeny.PhyBin.CoreTypes
       (
         NewickTree(..), displayDefaultTree,
         DefDecor, StandardDecor(..), treeSize, numLeaves,
         get_dec, set_dec, get_children, 
         map_labels, all_labels, foldIsomorphicTrees,

         -- Utilities specific to StandardDecor:
         avg_branchlen, get_bootstraps,
         

         toLabel, fromLabel, Label,
       )
       where

import Data.Maybe (maybeToList)

----------------------------------------------------------------------------------------------------
-- Type definitions
----------------------------------------------------------------------------------------------------

type BranchLen = Double

-- | Even though the Newick format allows it, here we ignore interior node
--   labels. (They are not commonly used.)
data NewickTree a = 
   NTLeaf     a Label
 | NTInterior a [NewickTree a]
 deriving (Show, Eq, Ord)

{-
-- [2010.09.22] Disabling:
instance NFData Atom where
  rnf a = rnf (fromAtom a :: Int)

instance NFData a => NFData (NewickTree a) where
  rnf (NTLeaf l n)      = rnf (l,n)
  rnf (NTInterior l ls) = rnf (l,ls)
-}

instance Functor NewickTree where 
   fmap fn (NTLeaf dec x)      = NTLeaf (fn dec) x 
   fmap fn (NTInterior dec ls) = NTInterior (fn dec) (map (fmap fn) ls)



----------------------------------------------------------------------------------------------------
-- Labels
----------------------------------------------------------------------------------------------------


type Label = String


----------------------------------------------------------------------------------------------------
-- Tree metadata (decorators)
----------------------------------------------------------------------------------------------------


-- | The default decorator for NewickTrees contains BOOTSTRAP and BRANCHLENGTH.
--   The bootstrap values, if present, will range in [0..100]
type DefDecor = (Maybe Int, BranchLen)


-- | The standard decoration includes everything in `DefDecor` plus
--   some extra cached data:
-- 
--  (1) branch length from parent to "this" node
--  (2) bootstrap values for the node
-- 
--  (3) subtree weights for future use
--      (defined as number of LEAVES, not counting intermediate nodes)
--  (4) sorted lists of labels for symmetry breaking
data StandardDecor = StandardDecor {
  branchLen     :: BranchLen,
  bootStrap     :: Maybe Int,

  -- The rest of these are used by the computations below.  These are
  -- cached (memoized) values that could be recomputed:
  ----------------------------------------
  subtreeWeight :: Int,
  sortedLabels  :: [Label]
 }
 deriving (Show,Read,Eq,Ord)



----------------------------------------------------------------------------------------------------
-- * Simple utility functions for the core types:
----------------------------------------------------------------------------------------------------

-- | How many leaf nodes (leaves and interior) are contained in a NewickTree?
treeSize :: NewickTree a -> Int
treeSize (NTLeaf _ _) = 1
treeSize (NTInterior _ ls) = 1 + sum (map treeSize ls)

-- | This counts only leaf nodes.
numLeaves :: NewickTree a -> Int
numLeaves (NTLeaf _ _) = 1
numLeaves (NTInterior _ ls) = sum (map numLeaves ls)


map_but_last :: (a -> a) -> [a] -> [a]
map_but_last _ [] = []
map_but_last _ [h] = [h]
map_but_last fn (h:t) = fn h : map_but_last fn t



get_dec :: NewickTree t -> t
get_dec (NTLeaf     dec _) = dec
get_dec (NTInterior dec _) = dec

-- Set all the decorations to a constant:
set_dec :: b -> NewickTree a -> NewickTree b
set_dec d = fmap (const d)
--set_dec d (NTLeaf _ x) = NTLeaf d x
--set_dec d (NTInterior _ ls) = NTInterior d $ map (set_dec d) ls

get_children :: NewickTree t -> [NewickTree t]
get_children (NTLeaf _ _) = []
get_children (NTInterior _ ls) = ls


-- | Average branch length across all branches in all all trees.
avg_branchlen :: [NewickTree StandardDecor] -> Double
avg_branchlen origls = fst total / snd total
  where
   total = sum_ls $ map sum_tree origls
   sum_ls ls = (sum$ map fst ls, sum$ map snd ls)
   sum_tree (NTLeaf (StandardDecor{branchLen=0}) _)    = (0,0)
   sum_tree (NTLeaf (StandardDecor{branchLen}) _)      = (abs branchLen,1)
   sum_tree (NTInterior (StandardDecor{branchLen}) ls) = 
       let (x,y) = sum_ls$ map sum_tree ls in
       if branchLen == 0 then (x, y) else ((abs branchLen) + x, 1+y)

-- | Retrieve all the bootstraps values actually present in a tree.
get_bootstraps :: NewickTree StandardDecor -> [Int]
get_bootstraps (NTLeaf (StandardDecor{bootStrap}) _) = maybeToList bootStrap
get_bootstraps (NTInterior (StandardDecor{bootStrap}) ls) =
  maybeToList bootStrap ++ concatMap get_bootstraps ls

-- | Apply a function to all the *labels* (leaf names) in a tree.
map_labels :: (Label -> Label) -> NewickTree a -> NewickTree a
map_labels fn (NTLeaf     dec lbl) = NTLeaf dec $ fn lbl
map_labels fn (NTInterior dec ls)  = NTInterior dec$ map (map_labels fn) ls
 
-- | Return all the labels contained in the tree.
all_labels :: NewickTree t -> [Label]
all_labels (NTLeaf     _ lbl) = [lbl]
all_labels (NTInterior _ ls)  = concat$ map all_labels ls



-- | This function allows one to collapse multiple trees while looking
-- only at the "horizontal slice" of all the annotations *at a given
-- position* in the tree.
--
-- "Isomorphic" must apply both to the shape and the name labels or it
-- is an error to apply this function.
foldIsomorphicTrees :: ([a] -> b) -> [NewickTree a] -> NewickTree b
foldIsomorphicTrees _ [] = error "foldIsomorphicTrees: empty list of input trees"
foldIsomorphicTrees fn ls@(hd:_) = fmap fn horiztrees
 where
   -- Preserve the input order:
   horiztrees = foldr consTrees (fmap (const []) hd) ls
   -- We use the tree datatype itself as the intermediate data
   -- structure.  This is VERY allocation-expensive, it would be
   -- possible to trade compute for allocation here:
   consTrees a b = case (a,b) of
    (NTLeaf dec nm1, NTLeaf decls nm2) | nm1 /= nm2 -> error$"foldIsomorphicTrees: mismatched names: "++show (nm1,nm2)
                                       | otherwise ->
     NTLeaf (dec : decls) nm1
    (NTInterior dec ls1, NTInterior decls ls2) ->
     NTInterior (dec:decls) $ zipWith consTrees ls1 ls2
    _ -> error "foldIsomorphicTrees: difference in tree shapes"

