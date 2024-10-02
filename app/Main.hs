module Main (main, readKey) where

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

data InputMode
  = IMNormal
  | IMAwaitBracket
  | IMAwaitTilde Char
  | IMAwaitControl
  deriving (Show)

data Key
  = KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyPgUp
  | KeyPgDown
  | KeyHome
  | KeyEnd
  | Key Char
  | KeyUnknown InputMode Char
  deriving (Show)

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

readKey :: InputMode -> IO Key
readKey mode =
  case mode of
    IMNormal -> do
      c <- getChar
      case c of
        '\ESC' -> readKey IMAwaitBracket
        x -> return (Key x)
    IMAwaitBracket -> do
      c <- getChar
      case c of
        '[' -> readKey IMAwaitControl
        _ -> return (KeyUnknown IMAwaitBracket c)
    IMAwaitControl -> do
      c <- getChar
      case c of
        'D' -> return KeyLeft
        'C' -> return KeyRight
        'A' -> return KeyUp
        'B' -> return KeyDown
        '5' -> readKey (IMAwaitTilde '5')
        '6' -> readKey (IMAwaitTilde '6')
        _ -> return (KeyUnknown IMAwaitControl c)
    IMAwaitTilde x -> do
      c <- getChar
      case c of
        '~' -> case x of
          '5' -> return KeyUp
          '6' -> return KeyDown
          _ -> return (KeyUnknown (IMAwaitTilde x) c)
        _ -> return (KeyUnknown (IMAwaitTilde x) c)
