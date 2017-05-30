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

data Deque a = MkDeque Int [a] [a] Int [a] [a]

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

emptyDEQ = MkDeque 0 [] [] 0 [] []
isEmptyDEQ (MkDeque lenf f sf lenr r sr) = (lenf + lenr) == 0
lengthDEQ (MkDeque lenf f sf lenr r sr) = lenf + lenr
firstDEQ _ = Nothing
lastDEQ _ = Nothing
takeFrontDEQ _ _ = []
takeBackDEQ _ _ = []
pushFrontDEQ _ _ = MkDeque 0 [] [] 0 [] []
popFrontDEQ _ = Nothing
pushBackDEQ _ _ = MkDeque 0 [] [] 0 [] []
popBackDEQ _ = Nothing
fromListDEQ _ = MkDeque 0 [] [] 0 [] []
