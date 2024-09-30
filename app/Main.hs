module Main where

import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import System.Console.ANSI (clearScreen, setCursorPosition)
import Text.Read (readMaybe)

type Model = Int

dataFile :: FilePath
dataFile = "taska.txt"

main :: IO ()
main = do
  databaseStr <- readFile dataFile
  let db = readMaybe databaseStr
  case db of
    Just s -> loop s
    _ -> putStrLn "Corrupt database file."

loop :: Model -> IO ()
loop model = do
  view model
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin LineBuffering
  putStrLn ""
  newModel <- update c model
  if newModel == 0
    then putStrLn "Bye."
    else do
      persistModel newModel
      loop newModel

persistModel :: Model -> IO ()
persistModel model =
  writeFile dataFile (show model)

update :: Char -> Model -> IO Model
update c model
  | c == 'u' = return (model + 1)
  | c == 'd' = return (model - 1)
  | c == 'q' = return 0
  | otherwise = return model

view :: Model -> IO ()
view model = do
  clearScreen
  setCursorPosition 0 0
  putStrLn ("The model is " ++ show model)
  putStrLn ""
  putStrLn ""
  putStrLn "'u' to up, 'd' to down, 'q' to quit. Value of 0 also quits."
