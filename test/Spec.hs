import Test.HUnit
import Deque


someList = [1, 6, 4, 2, 6, 3, 7]
-- empty deque equality
test1 = TestCase (assertEqual "empty deque length" True (isEmptyDEQ emptyDEQ) ) 

-- non-empty deque length
test2 = TestCase (assertEqual "non-empty deque length" False (isEmptyDEQ $ fromListDEQ $ someList) )

-- length of empty deque
test3 = TestCase (assertEqual "complex empty deque length" True (isEmptyDEQ $ getDeque $ popFrontDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [1, 2]) ) 

-- fromList and toList methods
test4 = TestCase (assertEqual "deque from list and to list" someList (toListDEQ $ fromListDEQ $ someList) ) 

-- length of non-empty deque
test5 = TestCase (assertEqual "deque length" 3 (lengthDEQ $ fromListDEQ $ [3, 4, 5]) ) 

-- first element from non-empty deque
test6 = TestCase (assertEqual "first of non-empty deque" (Just 5) (firstDEQ $ fromListDEQ $ [5, 2, 6, 3, 1]) )

-- first element from empty deque
test7 = TestCase (assertEqual "first of empty deque" (Nothing :: Maybe Int) (firstDEQ $ fromListDEQ $ ([] :: [Int])) )

-- last element from non-empty deque
test8 = TestCase (assertEqual "last of non-empty deque" (Just 1) (lastDEQ $ fromListDEQ $ [5, 2, 6, 3, 1]) )

-- last element from empty deque
test9 = TestCase (assertEqual "first of empty deque" (Nothing :: Maybe Int) (lastDEQ $ fromListDEQ $ ([] :: [Int])) )

-- front of empty deque
test10 = TestCase (assertEqual "take front from from empty deque" 0 (length $ (takeFrontDEQ 1 (fromListDEQ $ []))) )

-- front of non-empty deque
test11 = TestCase (assertEqual "take front from from non-empty deque" ['a', 'b', 'c']  (takeFrontDEQ 3 (fromListDEQ $ ['a', 'b', 'c', 'd'])) )

-- back from empty deque
test12 = TestCase (assertEqual "take back from from empty deque" 0 (length $ (takeBackDEQ 1 (fromListDEQ $ []))))

-- back from non-empty deque
test13 = TestCase (assertEqual "take back from non-empty deque" ['d', 'c', 'b']  (takeBackDEQ 3 (fromListDEQ $ ['a', 'b', 'c', 'd'])) )


-- multiple push front and push back
test14 = TestCase (assertEqual "push front and push back equality"   (pushFrontDEQ (pushFrontDEQ (pushFrontDEQ emptyDEQ 1) 2) 3) 
    (pushBackDEQ (pushBackDEQ (pushBackDEQ emptyDEQ 3) 2) 1) )

-- push front 
test15 = TestCase (assertEqual "push front" [1, 2, 3]  (toListDEQ (pushFrontDEQ (pushFrontDEQ (pushFrontDEQ emptyDEQ 3) 2) 1)) )

-- push back
test16 = TestCase (assertEqual "push back" [1, 2, 3]  (toListDEQ (pushBackDEQ (pushBackDEQ (pushBackDEQ emptyDEQ 1) 2) 3)) )

-- pop front
test17 = TestCase (assertEqual "pop front" [3] (toListDEQ $ getDeque $ popFrontDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [1, 2, 3]) )

-- pop back
test18 = TestCase (assertEqual "pop back" [1] (toListDEQ $ getDeque $ popBackDEQ $ getDeque $ popBackDEQ $ fromListDEQ $ [1, 2, 3]) )

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4,
                  TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8,
                  TestLabel "test9" test9, TestLabel "test10" test10, TestLabel "test11" test11, TestLabel "test12" test12,
                  TestLabel "test13" test13, TestLabel "test14" test14, TestLabel "test15" test15, TestLabel "test16" test16,
                  TestLabel "test17" test17, TestLabel "test18" test18]

main = runTestTT tests
