import Test.HUnit
import Deque


someList = [1, 6, 4, 2, 6, 3, 7]
test1 = TestCase (assertEqual "empty list length" True (isEmptyDEQ emptyDEQ)) 
test2 = TestCase (assertEqual "non-empty list length" False (isEmptyDEQ $ fromListDEQ $ someList))
test3 = TestCase (assertEqual "complex empty list length" True (isEmptyDEQ $ getDeque $ popFrontDEQ $ getDeque $ popFrontDEQ $ fromListDEQ $ [1, 2])) 
test4 = TestCase (assertEqual "deque from list and to list" someList (toListDEQ $ fromListDEQ $ someList)) 
test5 = TestCase (assertEqual "deque length" 3 (lengthDEQ $ fromListDEQ $ [3, 4, 5])) 
test6 = TestCase (assertEqual "first of non-empty list" (Just 5) (firstDEQ $ fromListDEQ $ [5, 2, 6, 3, 1]))
test7 = TestCase (assertEqual "first of empty list" (Nothing :: Maybe Int) (firstDEQ $ fromListDEQ $ ([] :: [Int])))
test8 = TestCase (assertEqual "last of non-empty list" (Just 1) (lastDEQ $ fromListDEQ $ [5, 2, 6, 3, 1]))
test9 = TestCase (assertEqual "first of empty list" (Nothing :: Maybe Int) (lastDEQ $ fromListDEQ $ ([] :: [Int])))
test10 = TestCase (assertEqual "take from from empty" 0 (length $ (takeFrontDEQ 1 (fromListDEQ $ []))))


tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4,
                  TestLabel "test5" test5, TestLabel "test6" test6, TestLabel "test7" test7, TestLabel "test8" test8,
                  TestLabel "test9" test9, TestLabel "test10" test10]
main = runTestTT tests
