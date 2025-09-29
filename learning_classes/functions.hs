length :: [t] -> INT
length [] = 0
length (a:as) = 1 + length as

(++) :: [t] -> [t] -> [t]
[] ++ y = y
(x:xs) ++ y = x : (xs ++ y)