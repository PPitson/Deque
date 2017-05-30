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

data Deque a = MkDeque Int [a] [a] Int [a] [a] deriving Show

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
popBackDEQ :: Deque a -> Maybe (a, Deque a)
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

takeFrontDEQ i (MkDeque lenf f sf lenr r sr)
    | i <= lenf = take i f
    | otherwise = take i (f ++ reverse r)

takeBackDEQ i (MkDeque lenf f sf lenr r sr)
    | i <= lenr = take i r
    | otherwise = take i (r ++ reverse f)

pushFrontDEQ (MkDeque lenf f sf lenr r sr) elem = balance (MkDeque (lenf+1) (elem:f) (drop 2 sf) lenr r (drop 2 sr))

popFrontDEQ (MkDeque lenf f sf lenr r sr) = case f of
	[] -> case r of
		[] -> Nothing
		(x : tr) -> Just (x, emptyDEQ)
	(x : tf) -> Just (x, balance (MkDeque (lenf - 1) tf tf lenr r sr))

pushBackDEQ (MkDeque lenf f sf lenr r sr) elem = balance (MkDeque lenf f (drop 2 sf) (lenr+1) (elem:r) (drop 2 sr))

popBackDEQ (MkDeque lenf f sf lenr r sr) = case r of
	[] -> case f of
		[] -> Nothing
		(x : tf) -> Just (x, emptyDEQ)
	(x : tr) -> Just (x, balance (MkDeque lenf f sf (lenr-1) tr tr))

fromListDEQ [] = emptyDEQ
fromListDEQ (x : t) = MkDeque lenf f sf lenr r sr
  where list = x : t
        (f, r) = splitAt (length list `div` 2) list
        lenf = length f
        lenr = length r
        sf = tail f
        sr = tail r

balance :: Deque a -> Deque a
balance (MkDeque lenf f sf lenr r sr)
	| lenf > 2*lenr+1 =
		let i = (lenf+lenr) `div` 2
		    j = lenf + lenr -i
		    f' = take i f
		    r' = rotateDrop r i f
		in MkDeque i f' f' j r' r'
  | lenr > 2*lenf+1 =
		let j = (lenf+lenr) `div` 2
		    i = lenf + lenr - j
		    r' = take i r
		    f' = rotateDrop f i r
		in MkDeque i f' f' j r' r'
	| otherwise = MkDeque lenf f sf lenr r sr

rotateRev :: [a] -> [a] -> [a] -> [a]
rotateRev [] r a = reverse (r ++ a)
rotateRev f r [] = f ++ reverse r
rotateRev (x: f) r a = x : rotateRev f (drop 2 r) (reverse (take 2 r ++ a))

rotateDrop :: [a] -> Int -> [a] -> [a]
rotateDrop f 0 r = rotateRev f r []
rotateDrop f 1 r = rotateRev f (drop 1 r) []
rotateDrop (x: f) j r = x : rotateDrop f (j - 2) (drop 2 r)

getDeque :: Maybe (a, Deque a) -> Deque a
getDeque Nothing = emptyDEQ
getDeque (Just (x, d)) = d
