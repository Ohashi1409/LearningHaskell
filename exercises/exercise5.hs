isCrescent :: (INT -> INT) -> INT -> BOOL
isCrescent f 0 = True
isCrescent f n | f n >= f n-1 = isCrescent f n-1
               | otherwise = False