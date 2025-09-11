vendas :: Int -> Int
vendas 0 = 23
vendas 1 = 20
vendas 2 = 40
vendas 3 = 28
vendas 4 = 28
vendas 5 = 10
vendas 6 = 25
vendas 7 = 31
vendas 8 = 15
vendas 9 = 20
vendas n = 0

totalVendas :: Int -> Int
totalVendas n 
            | (n==0) = vendas 0
            | otherwise = totalVendas (n-1) + vendas n

maxVendas :: Int -> Int
maxVendas   n 
            | (n==0) = vendas 0 
            | otherwise = max (maxVendas (n-1)) (vendas n)

vendasIguais :: Int -> Int -> Int
vendasIguais s 0 | (vendas 0 == s) = 1
                 | otherwise = 0

vendasIguais s n | (vendas n == s) = 1 + vendasIguais (s) (n-1)
                 | otherwise = vendasIguais (s) (n-1)