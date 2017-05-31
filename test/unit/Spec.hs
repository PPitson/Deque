import Test.HUnit
import Deque


-- empty deque equality
test1 = TestCase (assertEqual "empty deque length" True (isEmptyDEQ $ emptyDEQ) )
test2 = TestCase (assertEqual "empty deque length" True (isEmptyDEQ $ getDeque $ popBackDEQ $ (pushFrontDEQ emptyDEQ 1)) )
test3 = TestCase (assertEqual "empty deque length" True (isEmptyDEQ $ getDeque $ popBackDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [3, 4]) )
test4 = TestCase (assertEqual "empty deque length" True (isEmptyDEQ $ fromListDEQ $ []) )

-- non-empty deque
test5 = TestCase (assertEqual "non-empty deque length" False (isEmptyDEQ $ fromListDEQ $ [3]) )
test6 = TestCase (assertEqual "non-empty deque length" False (isEmptyDEQ $ fromListDEQ $ [5, 2, 6, 7, 3]) )
test7 = TestCase (assertEqual "non-empty deque length" False (isEmptyDEQ $ pushFrontDEQ (getDeque $ popBackDEQ $ emptyDEQ) 1) )

-- length of empty deque
test8 = TestCase (assertEqual "complex empty deque length" True (isEmptyDEQ $ getDeque $ popFrontDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [1, 2]) )
test9 = TestCase (assertEqual "complex empty deque length" False (isEmptyDEQ $ getDeque $ popFrontDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [1, 2, 4]) )
test10 = TestCase (assertEqual "complex empty deque length" False (isEmptyDEQ $ pushFrontDEQ (getDeque $ popFrontDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [1]) 1) )

someList = [1, 6, 4, 2, 6, 3, 7]

-- fromList and toList methods
test11 = TestCase (assertEqual "deque from list and to list" someList (toListDEQ $ fromListDEQ $ someList) )
test12 = TestCase (assertEqual "deque from list and to list" True (null $ toListDEQ $ emptyDEQ) )
test13 = TestCase (assertEqual "deque from list and to list" True (isEmptyDEQ $ fromListDEQ $ toListDEQ $ emptyDEQ) )

-- length of deque
test14 = TestCase (assertEqual "deque length" 0 (lengthDEQ $ fromListDEQ $ []) )
test15 = TestCase (assertEqual "deque length" 1 (lengthDEQ $ fromListDEQ $ [3]) )
test16 = TestCase (assertEqual "deque length" 3 (lengthDEQ $ fromListDEQ $ [3, 4, 5]) )

-- first element from non-empty deque
test17 = TestCase (assertEqual "first of non-empty deque" (Just 4) (firstDEQ $ fromListDEQ $ [4]) )
test18 = TestCase (assertEqual "first of non-empty deque" (Just 5) (firstDEQ $ fromListDEQ $ [5, 2, 6, 3, 1]) )

-- first element from empty deque
test19 = TestCase (assertEqual "first of empty deque" (Nothing :: Maybe Int) (firstDEQ $ fromListDEQ $ ([] :: [Int])) )

-- last element from non-empty deque
test20 = TestCase (assertEqual "last of non-empty deque" (Just 4) (lastDEQ $ fromListDEQ $ [4]) )
test21 = TestCase (assertEqual "last of non-empty deque" (Just 1) (lastDEQ $ fromListDEQ $ [5, 2, 6, 3, 1]) )

-- last element from empty deque
test22 = TestCase (assertEqual "first of empty deque" (Nothing :: Maybe Int) (lastDEQ $ fromListDEQ $ ([] :: [Int])) )

-- front of empty deque
test23 = TestCase (assertEqual "take front from empty deque" 0 (length $ (takeFrontDEQ 1 (fromListDEQ $ []))) )

-- front of non-empty deque
test24 = TestCase (assertEqual "take front from non-empty deque" []  (takeFrontDEQ 0 (fromListDEQ $ ['a'])) )
test25 = TestCase (assertEqual "take front from non-empty deque" ['a']  (takeFrontDEQ 3 (fromListDEQ $ ['a'])) )
test26 = TestCase (assertEqual "take front from non-empty deque" ['a', 'b', 'c']  (takeFrontDEQ 3 (fromListDEQ $ ['a', 'b', 'c', 'd'])) )

-- back from empty deque
test27 = TestCase (assertEqual "take back from empty deque" 0 (length $ (takeBackDEQ 1 (fromListDEQ $ []))))

-- back from non-empty deque
test28 = TestCase (assertEqual "take back from non-empty deque" []  (takeBackDEQ 0 (fromListDEQ $ ['a'])) )
test29 = TestCase (assertEqual "take back from non-empty deque" ['d']  (takeBackDEQ 3 (fromListDEQ $ ['d'])) )
test30 = TestCase (assertEqual "take back from non-empty deque" ['d', 'c', 'b']  (takeBackDEQ 3 (fromListDEQ $ ['a', 'b', 'c', 'd'])) )


-- multiple push front and push back
test31 = TestCase (assertEqual "push front and push back equality"   (pushFrontDEQ (pushFrontDEQ (pushFrontDEQ emptyDEQ 1) 2) 3)
    (pushBackDEQ (pushBackDEQ (pushBackDEQ emptyDEQ 3) 2) 1) )
test32 = TestCase (assertEqual "push front and push back equality"   (pushFrontDEQ (pushBackDEQ (fromListDEQ $ [1, 2]) 3) 0)
    (pushFrontDEQ(pushBackDEQ (pushFrontDEQ (fromListDEQ $ [2]) 1) 3) 0) )
test33 = TestCase (assertEqual "push front and push back equality"   (pushBackDEQ emptyDEQ 42) (pushFrontDEQ emptyDEQ 42) )

-- push front
test34 = TestCase (assertEqual "push front" [1]  (toListDEQ $ (pushFrontDEQ emptyDEQ 1)) )
test35 = TestCase (assertEqual "push front" [1, 2, 3, 4]  (toListDEQ $ (pushFrontDEQ (fromListDEQ $ [2, 3, 4]) 1)) )
test36 = TestCase (assertEqual "push front" [1, 2, 3]  (toListDEQ (pushFrontDEQ (pushFrontDEQ (pushFrontDEQ emptyDEQ 3) 2) 1)) )

-- push back
test37 = TestCase (assertEqual "push back" [1]  (toListDEQ $ (pushBackDEQ emptyDEQ 1)) )
test38 = TestCase (assertEqual "push back" [1, 2, 3, 4]  (toListDEQ $ (pushBackDEQ (fromListDEQ $ [1, 2, 3]) 4)) )
test39 = TestCase (assertEqual "push back" [1, 2, 3]  (toListDEQ (pushBackDEQ (pushBackDEQ (pushBackDEQ emptyDEQ 1) 2) 3)) )

-- pop front
test40 = TestCase (assertEqual "pop front" (Nothing :: Maybe (Int, Deque Int)) (popFrontDEQ $ fromListDEQ $ ([] :: [Int])) )
test41 = TestCase (assertEqual "pop front" (Just (1, emptyDEQ)) (popFrontDEQ $ fromListDEQ $ [1]) )
test42 = TestCase (assertEqual "pop front" (Just (2, fromListDEQ $ [3])) (popFrontDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [1, 2, 3]) )

-- pop back
test43 = TestCase (assertEqual "pop back" (Nothing :: Maybe (Int, Deque Int)) (popBackDEQ $ fromListDEQ $ ([] :: [Int])) )
test44 = TestCase (assertEqual "pop back" (Just (1, emptyDEQ)) (popFrontDEQ $ fromListDEQ $ [1]) )
test45 = TestCase (assertEqual "pop back" (Just (2, fromListDEQ $ [1])) (popBackDEQ $ getDeque $ popBackDEQ $ fromListDEQ $ [1, 2, 3]) )



tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4,
                  TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8,
                  TestLabel "test9" test9, TestLabel "test10" test10, TestLabel "test11" test11, TestLabel "test12" test12,
                  TestLabel "test13" test13, TestLabel "test14" test14, TestLabel "test15" test15, TestLabel "test16" test16,
                  TestLabel "test17" test17, TestLabel "test18" test18, TestLabel "test19" test19, TestLabel "test20" test20,
                  TestLabel "test21" test21, TestLabel "test22" test22, TestLabel "test23" test23, TestLabel "test24" test24,
                  TestLabel "test25" test25, TestLabel "test26" test26, TestLabel "test27" test27, TestLabel "test28" test28,
                  TestLabel "test29" test29, TestLabel "test30" test30, TestLabel "test31" test31, TestLabel "test32" test32,
                  TestLabel "test33" test33, TestLabel "test34" test34, TestLabel "test35" test35, TestLabel "test36" test36,
                  TestLabel "test37" test37, TestLabel "test38" test38, TestLabel "test39" test39, TestLabel "test40" test40,
                  TestLabel "test41" test41, TestLabel "test42" test42, TestLabel "test43" test43, TestLabel "test44" test44,
                  TestLabel "test45" test45]

main = runTestTT tests
