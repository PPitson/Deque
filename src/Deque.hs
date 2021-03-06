-- |
-- Double-ended queue (deque) purely functional implementation
-- based on Chris Okasaki's "Purely functional data structures"
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
 , reverseDEQ
 , fromListDEQ
 , toListDEQ
 , getDeque
 , isBalanced
 ) where

-- |
-- Data type for deques
data Deque a = MkDeque Int [a] Int [a]

instance Show a => Show (Deque a) where
   show (MkDeque _ f _ r) = show (f ++ reverse r)

instance Eq a => Eq (Deque a) where
  (==) (MkDeque _ f1 _ r1) (MkDeque _ f2 _ r2) = (f1 ++ reverse r1) == (f2 ++ reverse r2)

instance Functor Deque where
  fmap g (MkDeque lenf f lenr r) = MkDeque lenf (fmap g f) lenr (fmap g r)

instance Foldable Deque where
  foldMap g (MkDeque _ f _ r) = foldMap g (f ++ reverse r)

-- |
-- Creates an empty deque
--
-- O(1)
emptyDEQ :: Deque a
emptyDEQ = MkDeque 0 [] 0 []

-- |
-- Returns true if deque is empty
--
-- O(1)
isEmptyDEQ :: Deque a -> Bool
isEmptyDEQ (MkDeque lenf _ lenr _) = (lenf + lenr) == 0

-- |
-- Returns length of deque
--
-- O(1)
lengthDEQ :: Deque a -> Int
lengthDEQ (MkDeque lenf _ lenr _) = lenf + lenr

-- |
-- Returns item from the front of deque, without removing it
--
-- O(1)
firstDEQ :: Deque a -> Maybe a
firstDEQ (MkDeque _ [] _ []) = Nothing
firstDEQ (MkDeque _ [] _ (x:tr)) = Just x
firstDEQ (MkDeque _ (x:tf) _ _) = Just x

-- |
-- Returns item from the back of deque, without removing it
--
-- O(1)
lastDEQ :: Deque a -> Maybe a
lastDEQ (MkDeque _ [] _ []) = Nothing
lastDEQ (MkDeque _ (x:tf) _ []) = Just x
lastDEQ (MkDeque _ _ _ (x:tr)) = Just x

-- |
-- Returns first n elements of deque
--
-- O(n)
takeFrontDEQ :: Int -> Deque a -> [a]
takeFrontDEQ i (MkDeque lenf f _ r)
    | i <= lenf = take i f
    | otherwise = take i (f ++ reverse r)

-- |
-- Returns last n elements of deque
--
-- O(n)
takeBackDEQ :: Int -> Deque a -> [a]
takeBackDEQ i (MkDeque _ f lenr r)
    | i <= lenr = take i r
    | otherwise = take i (r ++ reverse f)

-- |
-- Pushes element to the front of deque and returns updated deque
--
-- O(1) amortised
pushFrontDEQ :: Deque a -> a -> Deque a
pushFrontDEQ (MkDeque lenf f lenr r) elem = balance (MkDeque (lenf+1) (elem:f) lenr r)

-- |
-- Pops element from the front of deque and returns it alongside updated deque
--
-- O(1) amortised
popFrontDEQ :: Deque a -> Maybe (a, Deque a)
popFrontDEQ (MkDeque _ [] _ []) = Nothing
popFrontDEQ (MkDeque _ [] _ (x:tr)) = Just (x, emptyDEQ)
popFrontDEQ (MkDeque lenf (x:tf) lenr r) = Just (x, balance (MkDeque (lenf - 1) tf lenr r))

-- |
-- Pushes element to the back of deque and returns updated deque
--
-- O(1) amortised
pushBackDEQ :: Deque a -> a -> Deque a
pushBackDEQ (MkDeque lenf f lenr r) elem = balance (MkDeque lenf f (lenr+1) (elem:r))

-- |
-- Pops element from the back of deque and returns it alongside updated deque
--
-- O(1) amortised
popBackDEQ :: Deque a -> Maybe (a, Deque a)
popBackDEQ (MkDeque _ [] _ []) = Nothing
popBackDEQ (MkDeque _ (x:tf) _ []) = Just (x, emptyDEQ)
popBackDEQ (MkDeque lenf f lenr (x:tr)) = Just (x, balance (MkDeque lenf f (lenr-1) tr))

-- |
-- Returns reversed deque
--
-- O(1)
reverseDEQ :: Deque a -> Deque a
reverseDEQ (MkDeque lenf f lenr r) = MkDeque lenr r lenf f

-- |
-- Creates deque from a list
--
-- O(n)
fromListDEQ :: [a] -> Deque a
fromListDEQ [] = emptyDEQ
fromListDEQ (x : t) = MkDeque lenf f lenr r
  where list = x : t
        (f, r') = splitAt (length list `div` 2) list
        r = reverse r'
        lenf = length f
        lenr = length r

-- |
-- Creates list from a deque
--
-- O(n)
toListDEQ :: Deque a -> [a]
toListDEQ (MkDeque _ f _ r) = f ++ reverse r

-- |
-- Gets deque from result of either pop function
--
-- O(1)
getDeque :: Maybe (a, Deque a) -> Deque a
getDeque Nothing = emptyDEQ
getDeque (Just (x, d)) = d

-- |
-- Returns true if deque is balanced
--
-- O(1)
isBalanced :: Deque a -> Bool
isBalanced (MkDeque lenf _ lenr _ ) = (lenf <= 2 * lenr + 1) || (lenr <= 2 * lenf + 1)


balance :: Deque a -> Deque a
balance (MkDeque lenf f lenr r)
    | lenf > 2*lenr+1 =
        let i = (lenf+lenr) `div` 2
            j = lenf + lenr - i
            f' = take i f
            r' = r ++ reverse (drop i f)
        in MkDeque i f' j r'
    | lenr > 2*lenf+1 =
        let i = (lenf+lenr) `div` 2
            j = lenf + lenr - i
            f' = f ++ reverse (drop j r)
            r' = take j r
        in MkDeque i f' j r'
    | otherwise = MkDeque lenf f lenr r
