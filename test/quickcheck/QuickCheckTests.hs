{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Data.Maybe
import Deque


instance (Arbitrary a) => Arbitrary (Deque a) where
    arbitrary = fmap fromListDEQ arbitrary

-- Checks if, after pushing x to the front of a deque, its first element is x
prop_PushFront :: Deque Int -> Int -> Bool
prop_PushFront d x = firstDEQ (pushFrontDEQ d x) == Just x

-- Checks if, after pushing x to the back of a deque, its last element is x
prop_PushBack :: Deque Int -> Int -> Bool
prop_PushBack d x = lastDEQ (pushBackDEQ d x) == Just x

-- Checks if, after pushing x to the front of a deque and popping from the front, the result is an initial deque
prop_PushPopFront ::  Deque Int -> Int -> Bool
prop_PushPopFront d x = getDeque (popFrontDEQ (pushFrontDEQ d x)) == d

-- Checks if, after pushing x to back front of a deque and popping from the back, the result is an initial deque
prop_PushPopBack ::  Deque Int -> Int -> Bool
prop_PushPopBack d x = getDeque (popBackDEQ (pushBackDEQ d x)) == d

-- Checks if, after pushing every element from a list to the front of a deque, takeFront results in an initial list
prop_TakeFront :: Deque Int -> [Int] -> Bool
prop_TakeFront d list = takeFrontDEQ (length list) (foldr (flip pushFrontDEQ) d list) == list

-- Checks if, after pushing every element from a list to the back of a deque, takeBack results in an initial list
prop_TakeBack :: Deque Int -> [Int] -> Bool
prop_TakeBack d list = takeBackDEQ (length list) (foldr (flip pushBackDEQ) d list) == list

-- Checks if, after pushing every element from a list to the front of a deque, resulting deque is balanced
prop_BalancedAfterFrontPushes :: Deque Int -> [Int] -> Bool
prop_BalancedAfterFrontPushes d list = isBalanced (foldr (flip pushFrontDEQ) d list)

-- Checks if, after pushing every element from a list to the back of a deque, resulting deque is balanced
prop_BalancedAfterBackPushes :: Deque Int -> [Int] -> Bool
prop_BalancedAfterBackPushes d list = isBalanced (foldr (flip pushBackDEQ) d list)

-- Checks if reversing a deque twice results in an initial deque
prop_Reverse :: Deque Int -> Bool
prop_Reverse d = reverseDEQ (reverseDEQ d) == d

-- Checks if creating deque from list and then list from deque results in an initial list
prop_ToFromList :: [Int] -> Bool
prop_ToFromList list = toListDEQ (fromListDEQ list) == list

-- Checks if creating list from deque and then deque from list results in an initial deque
prop_FromToList :: Deque Int -> Bool
prop_FromToList d = fromListDEQ (toListDEQ d) == d

-- Checks if deque's length increments by 1 after pushing to the front
prop_LengthAfterPushFront :: Deque Int -> Int -> Bool
prop_LengthAfterPushFront d x = lengthDEQ d == lengthDEQ (pushFrontDEQ d x) - 1

-- Checks if deque's length increments by 1 after pushing to the back
prop_LengthAfterPushBack :: Deque Int -> Int -> Bool
prop_LengthAfterPushBack d x = lengthDEQ d == lengthDEQ (pushBackDEQ d x) - 1

-- Checks if length of a deque created from a list is equal to list's length
prop_LengthAfterFromList :: [Int] -> Bool
prop_LengthAfterFromList list = lengthDEQ (fromListDEQ list) == length list

-- Checks if length of a list created from a deque is equal to deque's length
prop_LengthAfterToList :: Deque Int -> Bool
prop_LengthAfterToList d = length (toListDEQ d) == lengthDEQ d

return []
main = $quickCheckAll
