module Main (main) where

import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

data Model
  = Model
  { tick :: Int,
    counter :: Int,
    addMode :: Bool
  }
  deriving (Show, Read)

data Msg = Increment | Decrement | AddMode | Quit | Nop deriving (Show, Eq)

dataFile :: FilePath
dataFile = "taska.txt"

initModel :: Model
initModel =
  Model
    { tick = 0,
      counter = 0,
      addMode = False
    }

main :: IO ()
main = do
  databaseStr <- catchIOError (readFile dataFile) (\_ -> return (show initModel))
  let db = readMaybe databaseStr :: Maybe Model
  case db of
    Nothing -> putStrLn "Corrupt database file."
    Just s -> loop s

loop :: Model -> IO ()
loop model = do
  view model
  msg <- readMsg
  if msg == Quit
    then do
      persistModel model
      putStrLn "Bye"
    else do
      if msg == Nop
        then do
          loop model
        else do
          let newModel = update msg model
          persistModel newModel
          loop newModel {tick = tick newModel + 1}

readMsg :: IO Msg
readMsg = do
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin LineBuffering
  putStrLn ""
  return (toMsg c)

toMsg :: Char -> Msg
toMsg c = case c of
  'u' -> Increment
  'd' -> Decrement
  'a' -> AddMode
  'q' -> Quit
  _ -> Nop

persistModel :: Model -> IO ()
persistModel model =
  writeFile dataFile (show model)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Increment -> model {counter = counter model + 1}
    Decrement -> model {counter = counter model - 1}
    AddMode -> model {addMode = True}
    Quit -> model
    Nop -> model

view :: Model -> IO ()
view model = do
  clearScreen
  setCursorPosition 0 0
  putStrLn ("Current model is: " ++ show model)
  putStrLn ""
  putStrLn ""
  putStrLn "'u' to up, 'd' to down, 'q' to quit. Value of 0 also quits."
