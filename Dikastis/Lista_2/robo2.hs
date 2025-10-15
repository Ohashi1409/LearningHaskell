data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

faces :: Direction -> [Command] -> Direction
faces d [] = d
faces d (cmd : cmds) = faces (changeDirections d cmd) cmds

changeDirections :: Direction -> Command -> Direction
changeDirections d TurnLeft = case d of 
    North -> West
    West -> South
    South -> East
    East -> North
changeDirections d TurnRight = case d of 
    North -> East
    East -> South
    South -> West
    West -> North
changeDirections d _ = d

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result