vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 20
vendas 2 = 40
vendas 3 = 28
vendas 4 = 31
vendas n = 0

totalVendas :: Int -> Int
totalVendas n 
            | (n==0) = vendas 0
            | otherwise = totalVendas (n-1) + vendas n

maxVendas :: Int -> Int
maxVendas   n 
            | (n==0) = vendas 0 
            | otherwise = max (maxVendas (n-1)) (vendas n)