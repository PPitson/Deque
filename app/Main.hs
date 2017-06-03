module Main where

import Deque

main :: IO ()
main = do
    let res0 = fromListDEQ [4..10]
    print ("fromListDEQ --> " ++ show res0)

    let res1 = popFrontDEQ res0
    print ("popFrontDEQ --> " ++ show res1)
    let res2 = popFrontDEQ $ getDeque res1
    print ("popFrontDEQ --> " ++ show res2)

    let res3 = popBackDEQ $ getDeque res2
    print ("popBackDEQ --> " ++ show res3)
    let res4 = popBackDEQ $ getDeque res3
    print ("popBackDEQ --> " ++ show res4)

    let res5 = pushFrontDEQ (getDeque res4) 5
    print ("pushFrontDEQ 5 --> " ++ show res5) 
    let res6 = pushFrontDEQ res5 4
    print ("pushFrontDEQ 4 --> " ++ show res6) 

    let res7 = pushBackDEQ res6 9
    print ("pushFrontDEQ 9 --> " ++ show res7) 
    let res8 = pushBackDEQ res7 10
    print ("pushFrontDEQ 10 --> " ++ show res8) 

    print ("isEmptyDEQ --> " ++ show (isEmptyDEQ res8))

    print ("lengthDEQ --> " ++ show (lengthDEQ res8))

    print ("firstDEQ --> " ++ show (firstDEQ res8))

    print ("lastDEQ --> " ++ show (lastDEQ res8))

    print ("takeFrontDEQ 3 --> " ++ show (takeFrontDEQ 3 res8))

    print ("takeBackDEQ 3 --> " ++ show (takeBackDEQ 3 res8))
    
    print ("reverseDEQ --> " ++ show (reverseDEQ res8))

