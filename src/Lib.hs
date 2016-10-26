-- | SortedTree - https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/116718
module Lib
  (
    -- * Domain types
    Tree(..)
    -- * Exported functions
  , treeDepth
  , isSortedTree
  , addNewMax
  ) where

data Tree = Leaf | Node Int Tree Tree
  deriving Show

-- | Longest path from root to a leaf
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

-- | Is the tree sorted in-order?
isSortedTree :: Tree
             -> Int  -- ^ min for the current subtree
             -> Int  -- ^ max for the current subtree
             -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted

-- | Add a new max element to tree
-- | Will go down rightmost path to Leaf
addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree
