import Test.QuickCheck

prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

main = quickCheck prop_RevRev
