import Prelude hiding (Maybe (..))

import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond [_] = Nothing
safeSecond (_:x:xs) = Just (x)

main = do
       a <- getLine
       let result = safeSecond (read a::[Int])
       print result