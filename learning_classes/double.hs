double :: [INT] -> [INT]
double [] = []
double (x:xs) = (x*2) : double xs