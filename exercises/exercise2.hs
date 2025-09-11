ehprimo :: Int -> Bool
ehprimo 1 = False
ehprimo n = testaPrimo (n) (n-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo n 1 = True
testaPrimo n m | (n `mod` m == 0) = False
               | otherwise = testaPrimo (n) (m-1)