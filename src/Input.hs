module Input where

import Advent
import Data.Text

getInput :: Integer -> IO String
getInput n = do
  cookie <- readFile ".cookie"
  unpack <$> runAoC_ (opts cookie) (AoCInput (mkDay_ n))
    where opts c = defaultAoCOpts 2020 c