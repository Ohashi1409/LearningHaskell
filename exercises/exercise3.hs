primosEntreSi :: Int -> Int -> Bool
primosEntreSi a b 
                | (mdc a b == 1) = True
                | otherwise = False  

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)