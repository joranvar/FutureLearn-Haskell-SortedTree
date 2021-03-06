{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series

import Lib
-- instance (Monad m, Enum a, Bounded a) => Serial m a where
--   series = generate (\d -> take d [minBound .. maxBound])

import Control.Arrow ((&&&))
import Data.List (sort)

instance (Monad m) => Serial m Tree where
  series = cons0 Leaf \/ cons3 Node

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests =
  [ testProperty "Adding value to a sorted tree keeps it sorted" $
    \value treeValues -> let tree = foldl (flip addSortedValue) Leaf (treeValues::[Int])
                        in isSortedTree' tree && (isSortedTree' . addSortedValue value) tree
  , testProperty "Adding values to a sorted tree, then calling toList, returns the original values, sorted" $
    uncurry (==) . (toList . foldl (flip addSortedValue) Leaf &&& sort)
  ]

huTests :: [TestTree]
huTests =
  [ testCase "True is True" $
    True @?= True
  ]
