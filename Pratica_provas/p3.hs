insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (l:ls) | x > l = l : insert (x) (ls)
                | otherwise = x : l : ls

ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo x = testaPrimo (x) (x-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo x 1 = True
testaPrimo x y | (x `mod` y) == 0 = False
               | otherwise = testaPrimo (x) (y-1)

pegaPrimos1 :: Int -> Int -> [Int]
pegaPrimos1 x y = filter ehPrimo [x..y]

sumPrimeSquares1 :: Int -> Int -> Int
sumPrimeSquares1 x y = 
    let
        primos = pegaPrimos1 x y
        quadrados = map (^2) primos
    in 
        foldr (+) 0 quadrados

pegaPrimos2 :: Int -> Int -> [Int]
pegaPrimos2 x y = filter (\z -> ehPrimo z) [x..y]

sumPrimeSquares2 :: Int -> Int -> Int
sumPrimeSquares2 x y =
    let
        primos = pegaPrimos2
        quadrados = map (\x -> x*x) primos
    in 
        foldr (+) 0 quadrados

fibonnaci1 :: [Int]
fibonnaci1 = map (\x -> fib x) [0..]

fibonnaci2 :: [Int]
fibonnaci2 = map fib [0..]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

merge :: Ord t => [t] -> [t] -> [t]
merge _ y = y
merge x _ = x
merge (x:xs) (y:ys) | x <= y = x : merge (xs) (y:ys)
                    | otherwise = y : merge (x:xs) (ys)

mergesort :: Ord t => [t] -> [t]
mergesort [] = []
mergesort (x:xs) = merge [x] (mergesort xs)

