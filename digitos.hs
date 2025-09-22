digits :: String -> String
-- Alternativa: digits "" = ""
digits [] = []
digits (ch:chs) | ch >= '0' && ch <= '9' = ch : digits chs
                | otherwise = digits chs
