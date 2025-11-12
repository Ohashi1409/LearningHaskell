ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo x = testaPrimo (x) (x-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo x 1 = True
testaPrimo x y | (x `mod` y) == 0 = False
               | otherwise = testaPrimo (x) (y-1)

primos :: Int -> Int -> [Int]
primos x y = filter ehPrimo [x..y]

sumPrimeSquares :: Int -> Int -> Int
sumPrimeSquares start end =
    let 
        -- 1. Pega a lista de números primos no intervalo
        nums = primos start end
        -- 2. Eleva cada número primo ao quadrado
        quadrados = map (^2) nums
    in
        -- 3. Soma todos os quadrados usando foldr
        foldr (+) 0 quadrados