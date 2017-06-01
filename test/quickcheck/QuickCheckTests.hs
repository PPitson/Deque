{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Deque


instance (Arbitrary a) => Arbitrary (Deque a) where
    arbitrary = fmap fromListDEQ arbitrary

prop_PushPopFront ::  Deque Int -> Int -> Bool
prop_PushPopFront d x = getDeque (popFrontDEQ (pushFrontDEQ d x)) == d

prop_Push :: Deque Int -> Bool
prop_Push d = lengthDEQ d == lengthDEQ (pushFrontDEQ d 1) - 1


return []
main = $quickCheckAll
