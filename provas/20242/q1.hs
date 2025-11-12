insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (l:ls) | x > l = l : insert (x) (ls)
                | otherwise = x : (l:ls)