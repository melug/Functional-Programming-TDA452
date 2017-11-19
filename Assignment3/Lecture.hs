import Test.QuickCheck 

prop_example :: Integer -> Bool
prop_example n = (n+3)^2 == n^2+6*n+9
