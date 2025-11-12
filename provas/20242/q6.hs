data Tree = No Int Tree Tree | Folha Int deriving show 

teste1 = No 50 (No 25 (No 12 (Folha 18) (Folha 13)) (No 30 (Folha 26) (Folha 32))) (Folha 59)
teste2 = No 100 teste1 (No 200 (Folha 99) (No 298 (Folha 297) (Folha 299)))

ordenada :: Tree -> Bool
ordenada (Folha v) = True
ordenada (Node v a1 a2) = allLT v a1 && allGT v a2 && ordenada a1 && ordenada a2