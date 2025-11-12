import Data.Char
type Chave = [(Chave, Chave)]

letras :: [Char]
letras = ['A'..'Z']

cria_chave :: Int -> Chave
cria_chave x = 