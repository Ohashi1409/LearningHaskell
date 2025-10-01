btoi :: String -> Int
btoi [] = 0
btoi (s1:s1s) | s1 == '0' = btoi s1s
              | otherwise = (2 ^ length(s1s)) + btoi s1s

main = do
    s <- getLine
    let result = btoi s
    print result 