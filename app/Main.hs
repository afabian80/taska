module Main (main, whatKey) where

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

data Msg = Increment | Decrement | NormalMode | AddMode | Quit | Nop deriving (Show, Eq)

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
  '\ESC' -> NormalMode
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
    NormalMode -> model {addMode = False}
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

whatKey :: IO ()
whatKey = do
  c1 <- getChar
  putStrLn ""
  if c1 == '\ESC'
    then do
      c2 <- getChar
      if c2 == '['
        then do
          c3 <- getChar
          case c3 of
            'D' -> putStrLn "Left arrow"
            'C' -> putStrLn "Right arrow"
            'A' -> putStrLn "Up arrow"
            'B' -> putStrLn "Down arrow"
            '5' -> do
              c4 <- getChar
              case c4 of
                '~' -> putStrLn "Page up"
                _ -> putStrLn "Uknown c4 key"
            '6' -> do
              c4 <- getChar
              case c4 of
                '~' -> putStrLn "Page down"
                _ -> putStrLn "Uknown c4 key"
            'H' -> putStrLn "Home"
            'F' -> putStrLn "End"
            _ -> putStrLn "Unknown arrow key"
          whatKey
        else putStrLn "Unknown c2 key"
    else
      putStrLn $ "Key is " ++ show c1

-- putStrLn ""
-- let g = generalCategory c
-- putStrLn ("Key category is " ++ show g)
