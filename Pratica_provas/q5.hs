merge :: [Int] -> [Int] -> [Int]
merge _ y = y
merge x _ = x
merge (x:xs) (y:ys) | x <= y = x : merge (xs) (y:ys)
                    | otherwise = y : merge (x:xs) (ys)