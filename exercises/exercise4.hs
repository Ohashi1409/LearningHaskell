type BancoDados = [(Pessoa,Livro)]
type Pessoa = String
type Livro = String

-- livros emprestados
baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
               ("Andre","Duna"),
               ("Fernando","Jonathan Strange & Mr. Norrell"), 
               ("Fernando","Duna")
              ]
              
livros :: BancoDados -> Pessoa -> [Livro]
livros [] y = []
livros (x:xs) y | (fst x == y) = snd x : livros xs y
                | otherwise = livros xs y

emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos bd livro1 = [pess1 | (pess1, l1) <- bd, l1 == livro1]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd liv = ((emprestimos bd liv) /= [])

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd pess = length(livros bd pess)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd pess livr = (pess, livr) : bd

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] pess livr = []
devolver ((p1,l1):bd) pess livr | (p1, l1) == (pess, livr) = bd