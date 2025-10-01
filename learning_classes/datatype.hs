-- Construtores com Argumentos

-- data Shape = Circle Float 
--             | Rectangle Float Float
--             | Square Float

-- isRound :: Shape -> Bool
-- isRound (Circle _) = True
-- isRound (Rectangle _ _) = False
-- isRound (Square _) = False

-- area :: Shape -> Float
-- area (Circle r) = pi*r*r
-- area (Rectangle x y) = x*y
-- area (Square x) = x*x

-- Tipos recursivos

data Expr = Lit Int 
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval (e1) + eval (e2)
eval (Sub e1 e2) = eval (e1) + eval (e2)
eval (Mul e1 e2) = eval (e1) + eval (e2)
eval (Div e1 e2) = eval (e1) + eval (e2)

showExpr :: Expr -> String
showExpr (Lit n) = show n
showExpr (Add e1 e2) = "(" ++ showExpr(e1) ++ "+" ++ showExpr(e2) ++ ")"
showExpr (Sub e1 e2) = "(" ++ showExpr(e1) ++ "-" ++ showExpr(e2) ++ ")"
showExpr (Mul e1 e2) = "(" ++ showExpr(e1) ++ "*" ++ showExpr(e2) ++ ")"
showExpr (Div e1 e2) = "(" ++ showExpr(e1) ++ "/" ++ showExpr(e2) ++ ")"

data List t = Nil | Cons t (List t) deriving Show

toList :: List t -> [t]
toList Nil = []
toList (Cons x xs) = x : toList xs

fromList :: [t] -> List t
fromList [] = Nil
fromList (a:as) = Cons a fromList(as)