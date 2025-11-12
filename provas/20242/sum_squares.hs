ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo x = testaPrimo (x) (x-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo n 1 = True
testaPrimo n m | (n `mod` m) == 0 = False
               | otherwise = testaPrimo (n) (m-1)

pegaPrimos :: Int -> Int -> [Int]
pegaPrimos x y | 

sumPrimeSquares :: Int -> Int -> Int
sumPrimeSquares 

------------------------------------------------------
-- GPT
-- =========================================================
-- Suas funções para verificar se um número é primo
-- (A lógica está correta e será usada como base)
-- =========================================================
ehPrimo :: Int -> Bool
ehPrimo n | n < 2 = False -- Adicionando uma guarda para números menores que 2
          | otherwise = testaPrimo n (n-1)

testaPrimo :: Int -> Int -> Bool
testaPrimo n 1 = True
testaPrimo n m | n `mod` m == 0 = False
               | otherwise      = testaPrimo n (m-1)

-- =========================================================
-- Função para pegar os primos em um intervalo [x..y]
-- =========================================================
pegaPrimos :: Int -> Int -> [Int]
-- Usamos 'filter' com a sua função 'ehPrimo' na lista de números do intervalo.
pegaPrimos x y = filter ehPrimo [x..y]

-- =========================================================
-- Função principal para resolver o problema
-- =========================================================
sumPrimeSquares :: Int -> Int -> Int
sumPrimeSquares start end =
    let 
        -- 1. Pega a lista de números primos no intervalo
        primos = pegaPrimos start end
        -- 2. Eleva cada número primo ao quadrado
        quadrados = map (^2) primos
    in
        -- 3. Soma todos os quadrados usando foldr
        foldr (+) 0 quadrados

-- Para testar com o exemplo do enunciado:
main :: IO ()
main = print (sumPrimeSquares 30 50) -- Saída: 8069