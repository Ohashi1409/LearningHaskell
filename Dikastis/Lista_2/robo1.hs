data Command = Forward Int | Backward Int | TurnLeft | TurnRight
  deriving (Eq, Show, Read)

-- Direções possíveis
data Direction = North | East | South | West
  deriving (Eq, Show)

-- Função principal
destination :: (Int, Int) -> [Command] -> (Int, Int)
destination start cmds = move start North cmds
  where
    move :: (Int, Int) -> Direction -> [Command] -> (Int, Int)
    move pos _ [] = pos
    move (x, y) dir (cmd:rest) =
      case cmd of
        TurnLeft  -> move (x, y) (turnLeft dir) rest
        TurnRight -> move (x, y) (turnRight dir) rest
        Forward n -> move (forward (x, y) dir n) dir rest
        Backward n -> move (backward (x, y) dir n) dir rest

    turnLeft North = West
    turnLeft West  = South
    turnLeft South = East
    turnLeft East  = North

    turnRight North = East
    turnRight East  = South
    turnRight South = West
    turnRight West  = North

    forward (x, y) dir n =
      case dir of
        North -> (x, y + n)
        South -> (x, y - n)
        East  -> (x + n, y)
        West  -> (x - n, y)

    backward (x, y) dir n =
      case dir of
        North -> (x, y - n)
        South -> (x, y + n)
        East  -> (x - n, y)
        West  -> (x + n, y)
        
main = do
    a <- getLine
    b <- getLine
    let result = destination (read a) (read b)
    print result