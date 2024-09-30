module Main (main) where

import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

data Model
  = Model
  { tick :: Int,
    counter :: Int
  }
  deriving (Show, Read)

dataFile :: FilePath
dataFile = "taska.txt"

main :: IO ()
main = do
  databaseStr <- catchIOError (readFile dataFile) (\_ -> return (show Model {tick = 0, counter = 0}))
  let db = readMaybe databaseStr :: Maybe Model
  case db of
    Nothing -> putStrLn "Corrupt database file."
    Just s -> loop s

loop :: Model -> IO ()
loop model = do
  view model
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin LineBuffering
  putStrLn ""
  newModel <- update c model
  if counter newModel == 0
    then do
      persistModel newModel
      putStrLn "Bye."
    else do
      persistModel newModel
      loop newModel

persistModel :: Model -> IO ()
persistModel model =
  writeFile dataFile (show model)

update :: Char -> Model -> IO Model
update c model
  | c == 'u' = return (model {counter = counter model + 1})
  | c == 'd' = return (model {counter = counter model - 1})
  | c == 'q' = return (model {counter = 0})
  | otherwise = return model

view :: Model -> IO ()
view model = do
  clearScreen
  setCursorPosition 0 0
  putStrLn ("Current model is: " ++ show model)
  putStrLn ""
  putStrLn ""
  putStrLn "'u' to up, 'd' to down, 'q' to quit. Value of 0 also quits."
