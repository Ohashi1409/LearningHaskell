isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 _ = True
isReplica [] _ _ = False
isReplica (str:strs) x ch | str == ch = isReplica (strs) (x-1) ch
                          | otherwise = False

main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result