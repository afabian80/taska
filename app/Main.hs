{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}

module Main (main, readKey) where

import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import System.Console.ANSI (clearScreen, setCursorPosition) -- this is an external library
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

newtype Task
  = Task {title :: String}
  deriving (Show, Read, Eq)

data Screen = NormalScreen | AddTaskScreen deriving (Show, Eq, Read)

data Model
  = Model
  { tick :: Int,
    counter :: Int,
    logs :: [String],
    tasks :: [Task],
    screen :: Screen
  }
  deriving (Show, Read)

data InputMode
  = IMNormal
  | IMAwaitBracket
  | IMAwaitTilde Char
  | IMAwaitControl
  deriving (Show, Eq)

data Key
  = KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyPgUp
  | KeyPgDown
  | KeyHome
  | KeyEnd
  | KeyEsc
  | Key Char
  | KeyUnknown InputMode Char
  deriving (Show, Eq)

dataFile :: FilePath
dataFile = "taska.txt"

data Command = Nop | AddTask String deriving (Show, Eq)

data Msg = KeyMsg Key | CommandMsg Command deriving (Show, Eq)

initModel :: Model
initModel =
  Model
    { tick = 0,
      counter = 0,
      logs = [],
      tasks = [],
      screen = NormalScreen
    }

main :: IO ()
main = do
  databaseStr <- catchIOError (readFile dataFile) (\_ -> return (show initModel)) -- ignore all errors and start a fresh database
  let maybeModel = readMaybe databaseStr :: Maybe Model
  case maybeModel of
    Nothing -> putStrLn ("Corrupt database file. Delete " ++ dataFile ++ " to start a new database on model change.")
    Just model -> do
      let newModel = model {logs = []} -- clean logs when loading model database
      loop newModel

loop :: Model -> IO ()
loop model = do
  msg <- view model
  if msg == CommandMsg Nop
    then do
      hSetBuffering stdin NoBuffering -- so pressing Enter is not needed after character input
      key <- readKey IMNormal
      hSetBuffering stdin LineBuffering -- turn back the need for Enter and line buffering
      if key == Key 'q'
        then do
          persistModel model
          putStrLn "Bye"
        else do
          let newModel = update (KeyMsg key) model
          persistModel newModel
          loop newModel
    else do
      let newModel = update msg model
      persistModel newModel
      loop newModel

persistModel :: Model -> IO ()
persistModel model =
  writeFile dataFile (show model)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    KeyMsg key ->
      case key of
        KeyUp -> model {counter = counter model + 1}
        KeyDown -> model {counter = counter model - 1}
        KeyEsc -> model {screen = NormalScreen}
        Key 'a' -> model {screen = AddTaskScreen}
        KeyUnknown mode c -> model {logs = newLog : logs model}
          where
            newLog = "Unknown character " ++ show c ++ " in " ++ show mode ++ " mode."
        _ -> model
    CommandMsg command ->
      case command of
        AddTask s ->
          model
            { tasks = tasks model ++ [Task s],
              screen = NormalScreen,
              tick = tick model + 1
            }
        _ -> model

view :: Model -> IO Msg
view model = do
  case screen model of
    NormalScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn ("Current model is: " ++ show model)
      putStrLn ""
      putStrLn ""
      putStrLn "'Up arrow' to increment, Down arrow' to decrement, 'q' to quit."
      return (CommandMsg Nop)
    AddTaskScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "New task: "
      text <- getLine
      return (CommandMsg (AddTask text))

readKey :: InputMode -> IO Key
readKey mode =
  case mode of
    IMNormal -> do
      c <- getChar
      case c of
        '\ESC' -> readKey IMAwaitBracket -- a series of key codes encode the key, we need to further process
        x -> return (Key x) -- simples case for most keys
    IMAwaitBracket -> do
      c <- getChar
      case c of
        '[' -> readKey IMAwaitControl
        '\ESC' -> return KeyEsc
        _ -> return (KeyUnknown IMAwaitBracket c)
    IMAwaitControl -> do
      c <- getChar
      case c of
        'A' -> return KeyUp
        'B' -> return KeyDown
        'C' -> return KeyRight
        'D' -> return KeyLeft
        'F' -> return KeyEnd
        'H' -> return KeyHome
        '5' -> readKey (IMAwaitTilde '5')
        '6' -> readKey (IMAwaitTilde '6')
        _ -> return (KeyUnknown IMAwaitControl c)
    IMAwaitTilde x -> do
      c <- getChar
      case c of
        '~' -> case x of
          '5' -> return KeyPgUp
          '6' -> return KeyPgDown
          _ -> return (KeyUnknown (IMAwaitTilde x) c)
        _ -> return (KeyUnknown (IMAwaitTilde x) c)
