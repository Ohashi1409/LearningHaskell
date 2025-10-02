-- Função manual para fazer o split da string recebida --
split :: String -> [String]
split [] = [""] -- Caso da string vazia, retorna espaço em branco
split (c:cs) | c == ';'  = "" : split (cs) -- Se for ";", retorna espaço em branco e continua a recursão
             | otherwise = (c : head (split cs)) : tail (split cs) -- Se não for, pega o elemento e continua a recursão

-- Função que pega a string pós split e retorna uma lista com listas de 3 elementos (data, tipo, valor) --
agrupar3 :: [String] -> [(String, String, String)]
agrupar3 [] = [] -- Caso string vazia, retorna vazio
agrupar3 (d:t:v:resto) = (d, t, v) : agrupar3 resto -- Caso recursivo, agrupa em grupos de 3
agrupar3 _ = [] 

-- Função que obtem o mês a partir das sublistas de 3 elementos --
pegarMes :: String -> String
pegarMes dataCampo = last (words dataCampo)

-- Função principal --
logMes :: String -> String -> Double
logMes mes fatura =
    let campos = split fatura
        compras = agrupar3 campos
        valoresMes = [ read v :: Double | (dt, _, v) <- compras, mes == pegarMes dt ]
    in foldl (+) 0.0 valoresMes

-- Main --
main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result