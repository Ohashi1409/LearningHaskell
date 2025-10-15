data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

alturaArvore :: Tree t -> Int
alturaArvore Nilt = 0
alturaArvore (Node _ left right) = 1 + max (alturaArvore(left)) (alturaArvore (right))

maiorDiametro :: Ord t => Tree t -> Int
