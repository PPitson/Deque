-- |
-- Double-ended queue (deque) purely functional implementation using two lists
module Deque
 ( Deque
 , emptyDEQ
 , isEmptyDEQ
 , lengthDEQ
 , firstDEQ
 , lastDEQ
 , takeFrontDEQ
 , takeBackDEQ
 , pushFrontDEQ
 , popFrontDEQ
 , pushBackDEQ
 , popBackDEQ
 , fromListDEQ
 , toListDEQ
 , getDeque
 ) where

-- |
-- Deque is a 4-tuple (lenf, f, lenr, r), where lenf is length of list f,
-- f is a front list, lenr is a length of list r, r is a reversed rear list
data Deque a = MkDeque Int [a] Int [a]

instance Show a => Show (Deque a) where
   show (MkDeque _ f _ r) = show (f ++ reverse r)

instance Eq a => Eq (Deque a) where
  (==) (MkDeque _ f1 _ r1) (MkDeque _ f2 _ r2) = (f1 ++ r1) == (f2 ++ r2)

instance Functor Deque where
  fmap g (MkDeque lenf f lenr r) = MkDeque lenf (fmap g f) lenr (fmap g r)

-- |
-- O(1)
emptyDEQ :: Deque a
emptyDEQ = MkDeque 0 [] 0 []

-- |
-- O(1)
isEmptyDEQ :: Deque a -> Bool
isEmptyDEQ (MkDeque lenf f lenr r) = (lenf + lenr) == 0

-- |
-- O(1)
lengthDEQ :: Deque a -> Int
lengthDEQ (MkDeque lenf f lenr r) = lenf + lenr

-- |
-- O(1)
firstDEQ :: Deque a -> Maybe a
firstDEQ (MkDeque lenf f lenr r) = case f of
    [] -> case r of
        [] -> Nothing
        (x : tr) -> Just x
    (x : tf) -> Just x

-- |
-- O(1)
lastDEQ :: Deque a -> Maybe a
lastDEQ (MkDeque lenf f lenr r) = case r of
    [] -> case f of
        [] -> Nothing
        (x : tf) -> Just x
    (x : tr) -> Just x

-- |
-- O(n)
takeFrontDEQ :: Int -> Deque a -> [a]
takeFrontDEQ i (MkDeque lenf f lenr r)
    | i <= lenf = take i f
    | otherwise = take i (f ++ reverse r)

-- |
-- O(n)
takeBackDEQ :: Int -> Deque a -> [a]
takeBackDEQ i (MkDeque lenf f lenr r)
    | i <= lenr = take i r
    | otherwise = take i (r ++ reverse f)

-- |
-- O(1) amortised
pushFrontDEQ :: Deque a -> a -> Deque a
pushFrontDEQ (MkDeque lenf f lenr r) elem = balance (MkDeque (lenf+1) (elem:f) lenr r)

-- |
-- O(1) amortised
popFrontDEQ :: Deque a -> Maybe (a, Deque a)
popFrontDEQ (MkDeque lenf f lenr r) = case f of
    [] -> case r of
        [] -> Nothing
        (x : tr) -> Just (x, emptyDEQ)
    (x : tf) -> Just (x, balance (MkDeque (lenf - 1) tf lenr r))

-- |
-- O(1) amortised
pushBackDEQ :: Deque a -> a -> Deque a
pushBackDEQ (MkDeque lenf f lenr r) elem = balance (MkDeque lenf f (lenr+1) (elem:r))

-- |
-- O(1) amortised
popBackDEQ :: Deque a -> Maybe (a, Deque a)
popBackDEQ (MkDeque lenf f lenr r) = case r of
    [] -> case f of
        [] -> Nothing
        (x : tf) -> Just (x, emptyDEQ)
    (x : tr) -> Just (x, balance (MkDeque lenf f (lenr-1) tr))

-- |
-- O(n)
fromListDEQ :: [a] -> Deque a
fromListDEQ [] = emptyDEQ
fromListDEQ (x : t) = MkDeque lenf f lenr r
  where list = x : t
        (f, rr) = splitAt (length list `div` 2) list
        r = reverse rr
        lenf = length f
        lenr = length r


toListDEQ :: Deque a -> [a]
toListDEQ (MkDeque lenf f lenr r) = f ++ reverse r



balance :: Deque a -> Deque a
balance (MkDeque lenf f lenr r)
    | lenf > 2*lenr+1 =
        let i = (lenf+lenr) `div` 2
            j = lenf + lenr -i
            f' = take i f
            r' = rotateDrop r i f
        in MkDeque i f' j r'
    | lenr > 2*lenf+1 =
        let j = (lenf+lenr) `div` 2
            i = lenf + lenr - j
            r' = take i r
            f' = rotateDrop f i r
        in MkDeque i f' j r'
    | otherwise = MkDeque lenf f lenr r

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
