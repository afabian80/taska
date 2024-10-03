{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}

module Main (main, readKey) where

import qualified Data.Vector as V
import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

data Task
  = Task
  { title :: String,
    active :: Bool,
    lastTick :: Int
  }
  deriving (Show, Read, Eq)

data Screen = NormalScreen | AddTaskScreen deriving (Show, Eq, Read)

data Model
  = Model
  { tick :: Int,
    index :: Maybe Int,
    logs :: [String],
    tasks :: V.Vector Task,
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
      index = Nothing,
      logs = [],
      tasks = V.empty,
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
        KeyUp -> model {index = newIndex}
          where
            newIndex =
              fmap
                (\x -> max 0 (x - 1))
                (index model)
        KeyDown -> model {index = newIndex}
          where
            newIndex =
              fmap
                (\x -> min (length (tasks model) - 1) (x + 1))
                (index model)
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
            { tasks = V.snoc (tasks model) (Task s False (tick model)),
              screen = NormalScreen,
              tick = tick model + 1,
              index = Just 0
            }
        _ -> model

view :: Model -> IO Msg
view model = do
  case screen model of
    NormalScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "Tasks:"
      let tasksWithCursor = addCursor (tasks model) (index model)
      render tasksWithCursor
      putStrLn ""
      putStrLn "Keys: up, down, a and q."
      putStrLn ""
      putStrLn ("Current model is: " ++ show model)
      return (CommandMsg Nop)
    AddTaskScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "New task: "
      text <- getLine
      return (CommandMsg (AddTask text))

render :: V.Vector Task -> IO ()
render ts = do
  mapM_ printTask ts

printTask :: Task -> IO ()
printTask t =
  if active t
    then
      putStrLn ("> " ++ title t ++ " (" ++ show (lastTick t) ++ ")")
    else
      putStrLn ("  " ++ title t ++ " (" ++ show (lastTick t) ++ ")")

addCursor :: V.Vector Task -> Maybe Int -> V.Vector Task
addCursor vec Nothing = vec
addCursor vec (Just i) =
  case (V.!?) vec i of
    Nothing -> vec
    Just t -> V.update vec (V.singleton (i, t {active = True}))

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
