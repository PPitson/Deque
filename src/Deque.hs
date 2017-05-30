module Deque
 ( Deque
 , emptyDEQ     -- :: Deque a
 , isEmptyDEQ   -- :: Deque a -> Bool
 , lengthDEQ    -- :: Deque a -> Int, O(1)
 , firstDEQ     -- :: Deque a -> Maybe a,  O(1)
 , lastDEQ      -- :: Deque a -> Maybe a, O(1)
 , takeFrontDEQ -- :: Int -> Deque a -> [a], O(n)
 , takeBackDEQ  -- :: Int -> Deque a -> [a], O(n)
 , pushFrontDEQ -- :: Deque a -> a -> Deque a, O(1) amortised
 , popFrontDEQ  -- :: Deque a -> Maybe (a, Deque a), O(1) amortised
 , pushBackDEQ  -- :: Deque a -> a -> Deque a, O(1) amortised
 , popBackDEQ   -- :: Deque a -> Maybe (a, q a), O(1) amortised
 , fromListDEQ  -- :: [a] -> Deque a, O(n)
 ) where

emptyDEQ :: Deque a
isEmptyDEQ :: Deque a -> Bool
lengthDEQ :: Deque a -> Int
firstDEQ :: Deque a -> Maybe a
lastDEQ :: Deque a -> Maybe a
takeFrontDEQ :: Int -> Deque a -> [a]
takeBackDEQ :: Int -> Deque a -> [a]
pushFrontDEQ :: Deque a -> a -> Deque a
popFrontDEQ :: Deque a -> Maybe (a, Deque a)
pushBackDEQ :: Deque a -> a -> Deque a
popBackDEQ :: Deque a -> Maybe (a, q a)
fromListDEQ :: [a] -> Deque a
