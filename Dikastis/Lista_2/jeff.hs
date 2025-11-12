-- Mapeia uma nota alfabética para seu valor numérico
toNumeric :: String -> Double
toNumeric n = case n of
  "A+" -> 9.7
  "A"  -> 9.3
  "A-" -> 9.0
  "B+" -> 8.7
  "B"  -> 8.3
  "B-" -> 8.0
  "C+" -> 7.7
  "C"  -> 7.3
  "C-" -> 7.0
  "D+" -> 6.7
  "D"  -> 6.3
  "D-" -> 6.0
  "F"  -> 5.9
  _    -> 0.0  -- caso de entrada inválida

-- Converte valor numérico de volta para nota alfabética
toLetter :: Double -> String
toLetter x
  | x >= 9.7 = "A+"
  | x >= 9.3 = "A"
  | x >= 9.0 = "A-"
  | x >= 8.7 = "B+"
  | x >= 8.3 = "B"
  | x >= 8.0 = "B-"
  | x >= 7.7 = "C+"
  | x >= 7.3 = "C"
  | x >= 7.0 = "C-"
  | x >= 6.7 = "D+"
  | x >= 6.3 = "D"
  | x >= 6.0 = "D-"
  | otherwise = "F"

-- Função principal
contagemNotas :: [String] -> [String] -> Int
contagemNotas notas faculdades =
  let media = sum (map toNumeric notas) / fromIntegral (length notas)
      mediaNum = toNumeric (toLetter media)
  in length [f | f <- faculdades, mediaNum >= toNumeric f]

main = do
    a <- getLine
    b <- getLine
    print (contagemNotas (read a) (read b))