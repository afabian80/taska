module Main where

import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  loop 1

loop :: Int -> IO ()
loop model = do
  -- printf "Model is %d\n" model
  view model
  c <- getChar
  newModel <- update c model
  loop newModel

-- main :: IO ()
-- main = do
--   putStrLn "Try typing and deleting!"
--   s <- getLine
--   printf "You typed %s\n" s
--   putStrLn "Try presisng a key!"
--   hSetBuffering stdin NoBuffering
--   c <- getChar
--   printf "You typed %c\n" c
--   putStrLn "Done."