ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo x = testaPrimo (x) (x-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo x 1 = True
testaPrimo x y | (x `mod` y) == 0 = False
               | otherwise = testaPrimo (x) (y-1)