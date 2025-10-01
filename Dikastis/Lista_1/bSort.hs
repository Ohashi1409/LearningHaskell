bSort :: [String] -> [String]
bSort [] = []
bSort (str:strs) = bSort [temp | temp <- strs, temp < str] ++
                    [str] ++
                    bSort [temp | temp <- strs, temp >= str] 

main = do
       a <- getLine
       let result = bSort (read a :: [String])
       print result