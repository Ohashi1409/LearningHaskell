mul2 :: [Int] -> [Int] -> [Int]
-- Caso vazio
mul2 [] [] = []
-- Caso lista 1 vazia
mul2 (l1:l1s) [] = l1*0 : mul2 (l1s) []
-- Caso lista 2 vazia
mul2 [] (l2:l2s) = l2*0 : mul2 [] (l2s)
-- Caso lista 1 e 2 cheias 
mul2 (l1:l1s) (l2:l2s) = l1*l2 : mul2 (l1s) (l2s)

main = do
    sa <- getLine
    let a = read sa :: [Int]
    sb <- getLine
    let b = read sb :: [Int]
    let result = mul2 a b
    print result