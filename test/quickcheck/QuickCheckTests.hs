{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.Maybe
import Deque


instance (Arbitrary a) => Arbitrary (Deque a) where
    arbitrary = fmap fromListDEQ arbitrary

prop_PushFront :: Deque Int -> Int -> Bool
prop_PushFront d x = firstDEQ (pushFrontDEQ d x) == Just x

prop_PushBack :: Deque Int -> Int -> Bool
prop_PushBack d x = lastDEQ (pushBackDEQ d x) == Just x

prop_PushPopFront ::  Deque Int -> Int -> Bool
prop_PushPopFront d x = getDeque (popFrontDEQ (pushFrontDEQ d x)) == d

prop_PushPopBack ::  Deque Int -> Int -> Bool
prop_PushPopBack d x = getDeque (popBackDEQ (pushBackDEQ d x)) == d

prop_TakeFront :: Deque Int -> [Int] -> Bool
prop_TakeFront d list = takeFrontDEQ (length list) (foldr (flip pushFrontDEQ) d list) == list

prop_TakeBack :: Deque Int -> [Int] -> Bool
prop_TakeBack d list = takeBackDEQ (length list) (foldr (flip pushBackDEQ) d list) == list

prop_BalancedAfterFrontPushes :: Deque Int -> [Int] -> Bool
prop_BalancedAfterFrontPushes d list = isBalanced (foldr (flip pushFrontDEQ) d list)

prop_BalancedAfterBackPushes :: Deque Int -> [Int] -> Bool
prop_BalancedAfterBackPushes d list = isBalanced (foldr (flip pushBackDEQ) d list)

prop_ToFromList :: [Int] -> Bool
prop_ToFromList list = toListDEQ (fromListDEQ list) == list

prop_FromToList :: Deque Int -> Bool
prop_FromToList d = fromListDEQ (toListDEQ d) == d

prop_LengthAfterPushFront :: Deque Int -> Int -> Bool
prop_LengthAfterPushFront d x = lengthDEQ d == lengthDEQ (pushFrontDEQ d x) - 1

prop_LengthAfterPushBack :: Deque Int -> Int -> Bool
prop_LengthAfterPushBack d x = lengthDEQ d == lengthDEQ (pushBackDEQ d x) - 1

prop_LengthAfterFromList :: [Int] -> Bool
prop_LengthAfterFromList list = lengthDEQ (fromListDEQ list) == length list

prop_LengthAfterToList :: Deque Int -> Bool
prop_LengthAfterToList d = length (toListDEQ d) == lengthDEQ d

return []
main = $quickCheckAll
