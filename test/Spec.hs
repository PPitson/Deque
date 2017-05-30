import Test.HUnit


test1 = TestCase (assertEqual "arithmetic test" 8 (4+4)) 
test2 = TestCase (assertEqual "arithmetic test" 9 (4+4)) 

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
main = runTestTT tests
