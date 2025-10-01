ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo n = testaPrimo (n) (n-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo n 1 = True
testaPrimo n m | (n `mod` m) == 0 = False
               | otherwise = testaPrimo (n) (m-1)