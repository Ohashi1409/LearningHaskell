sumPairs :: [(INT, INT)] -> [Int]
sumPairs [] = []
sumPairs (pair:pairs) = (fst pair + snd pair) : sumPairs pairs