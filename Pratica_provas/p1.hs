insert :: Int -> [Int]
insert x [] = [x]
insert x (y:ys) | x <= y = x : (y:ys)
                | otherwise = y : insert (x) (ys)

ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo x = testaPrimo (x) (x-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo x 1 = True
testaPrimo x y | (x `mod` y) == 0 = False
               | otherwise = testaPrimo (x) (y-1)

pegaPrimos :: Int -> Int -> [Int]
pegaPrimos x y = filter ehPrimo [x..y]

sumPrimeSquares :: Int -> Int -> Int
sumPrimeSquares x y = 
    let 
        primos = pegaPrimos (x) (y)
        quadrados = map (^2) primos
    in 
        foldr (+) 0 quadrados 


fibonnaci :: [Int]
fibonnaci = map fib [0..]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

merge :: [Int] -> [Int] -> [Int]
merge _ y = y
merge x _ = x
merge (x:xs) (y:ys) | x <= y = x : merge (xs) (y:ys)
                    | otherwise = y : merge (x:xs) (ys)

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort (x:xs) = merge [x] (mergesort xs)