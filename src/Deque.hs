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

firstDEQ (MkDeque lenf f sf lenr r sr) = case f of
	[] -> case r of
		[] -> Nothing
		(x : tr) -> Just x
	(x : tf) -> Just x

lastDEQ (MkDeque lenf f sf lenr r sr) = case r of
	[] -> case f of
		[] -> Nothing
		(x : tf) -> Just x
	(x : tr) -> Just x

takeFrontDEQ _ _ = []

takeBackDEQ _ _ = []

pushFrontDEQ (MkDeque lenf f sf lenr r sr) elem = balance (MkDeque (lenf+1) (elem:f) (drop 2 sf) lenr r (drop 2 sr))
popFrontDEQ _ = Nothing
pushBackDEQ (MkDeque lenf f sf lenr r sr) elem = balance (MkDeque lenf f (drop 2 sf) (lenr+1) (elem:r) (drop 2 sr))
popBackDEQ _ = Nothing
fromListDEQ _ = MkDeque 0 [] [] 0 [] []


balance :: Deque a -> Deque a
balance (MkDeque lenf f sf lenr r sr) 
	| lenf > 2*lenr+1 = 
		let i = (lenf+lenr) `div` 2
			j = lenf + lenr -i
			f` = take i f
			r' = rotateDrop r i f
		in MkDeque i f' f' j r' r'
	| lenr > 2*lenf+1 = 
		let j = (lenf+lenr) `div` 2
			i = lenf + lenr - j
			r' = take i r
			f' = rotateDrop f i r
		in MkDeque i f' f' j r' r'
	| otherwise = MkDeque lenf f sf lenr r sr


rotateDrop :: [a] -> Int -> [a] -> [a]
rotateDrop = []