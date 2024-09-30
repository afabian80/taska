module Main where

import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hFlush, hSetBuffering)
import GHC.IO.Handle.FD (stdin, stdout)

type Model = Int

main :: IO ()
main = do
  let model = 1 :: Model
  loop model

loop :: Model -> IO ()
loop model = do
  view model
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin LineBuffering
  putStrLn ""
  newModel <- update c model
  if model == 0
    then putStrLn "Bye."
    else loop newModel

update :: Char -> Model -> IO Model
update c model
  | c == 'u' = return (model + 1)
  | c == 'd' = return (model - 1)
  | c == 'q' = return 0
  | otherwise = return model

view :: Model -> IO ()
view model = do
  putStrLn ("The model is " ++ show model)
  putStrLn "'u' to up, 'd' to down, 'q' to quit. Value of 0 also quits."
