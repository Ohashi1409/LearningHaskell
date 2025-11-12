data Cmd
  = Cursor Int
  | Backspace Int
  | Delete Int
  | Insert String
  deriving (Read, Show)

editText :: String -> [Cmd] -> String
editText texto comandos = go texto 0 comandos
  where
    go t _ [] = t
    go t pos (cmd:cs) =
      case cmd of
        Cursor x ->
          go t (pos + x) cs

        Backspace x ->
          let before = take (pos - x) t
              after = drop pos t
              newPos = pos - x
          in go (before ++ after) newPos cs

        Delete x ->
          let before = take pos t
              after = drop (pos + x) t
          in go (before ++ after) pos cs

        Insert s ->
          let before = take pos t
              after = drop pos t
              newPos = pos  -- o cursor aponta para o começo da string inserida
          in go (before ++ s ++ after) newPos cs

main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result