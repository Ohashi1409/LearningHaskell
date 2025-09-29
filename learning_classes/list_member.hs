member :: [INT] -> INT -> Bool
member [] a = False
member (x:xs) a |
                | x == a = True
                | otherwise = member xs a