-- Função manual para fazer o split da string recebida --
split :: String -> [String]
split [] = [""] -- Caso da string vazia, retorna espaço em branco
split (c:cs) | c == ';'  = "" : split (cs) -- Se for ";", retorna espaço em branco e continua a recursão
             | otherwise = (c : head (split cs)) : tail (split cs) -- Se não for, pega o elemento e continua a recursão

-- Função que recebe a string pós split e pega os valores --
pegarValores :: [String] -> [String]
pegarValores [] = [] --  Caso onde recebe uma lista vazia
pegarValores (_:_:v:resto) = v : pegarValores resto -- Caso recursivo 
pegarValores _ = [] -- Caso onde não possui 3 elementos ou entra num ";"

-- Função principal que pega os valores máximos e mínimos -- 
minMaxCartao :: String -> (Double, Double)
minMaxCartao input =
    let campos  = split input -- Faz o split do input
        valores = map read (pegarValores campos) :: [Double] -- Pega apenas os elementos que podem virar double
    in (minimum valores, maximum valores)

-- Main -- 
main = do
    a <- getLine
    let result = minMaxCartao a
    print result