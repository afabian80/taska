{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}

module Main (main, readKey) where

import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin)
import System.Console.ANSI (clearScreen, setCursorPosition)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

data TaskState
  = Todo
  | Started
  | Stopped
  | Done
  deriving (Show, Eq, Read)

data Task
  = Task
  { title :: String,
    active :: Bool,
    lastTick :: Int,
    state :: TaskState
  }
  deriving (Show, Read, Eq)

data Screen = NormalScreen | AddTaskScreen deriving (Show, Eq, Read)

data Model
  = Model
  { tick :: Int,
    index :: Maybe Int,
    logs :: [String],
    tasks :: [Task],
    screen :: Screen,
    compareTick :: Int
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
  | KeyDelete
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
      tasks = [],
      screen = NormalScreen,
      compareTick = 0
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
        Key 'u' -> model {compareTick = tick model}
        Key ' ' ->
          model
            { tasks = markDone (tasks model) (index model) (tick model),
              tick = tick model + 1
            }
        Key 't' ->
          model
            { tasks = markTodo (tasks model) (index model) (tick model),
              tick = tick model + 1
            }
        Key 's' ->
          model
            { tasks = markStartStop (tasks model) (index model) (tick model),
              tick = tick model + 1
            }
        KeyDelete ->
          model
            { tasks = newTasks,
              tick = tick model + 1,
              index = if not (null newTasks) then Just 0 else Nothing
            }
          where
            newTasks = deleteListIndexSafe (tasks model) (index model)
        KeyUnknown mode c -> model {logs = newLog : logs model}
          where
            newLog = "Unknown character " ++ show c ++ " in " ++ show mode ++ " mode."
        _ -> model
    CommandMsg command ->
      case command of
        AddTask s ->
          model
            { tasks = tasks model ++ [Task s False (tick model) Todo],
              screen = NormalScreen,
              tick = tick model + 1,
              index = Just 0
            }
        _ -> model

markDone :: [Task] -> Maybe Int -> Int -> [Task]
markDone = markState Done

markTodo :: [Task] -> Maybe Int -> Int -> [Task]
markTodo = markState Todo

markStartStop :: [Task] -> Maybe Int -> Int -> [Task]
markStartStop ts Nothing _ = ts
markStartStop ts (Just i) time =
  case elemAtIndex ts i of
    Nothing -> ts
    (Just t) -> replaceElemAtIndexIfExists ts i (t {state = newState (state t), lastTick = time})
  where
    newState Todo = Started
    newState Started = Stopped
    newState Stopped = Started
    newState Done = Done -- explicit Todo state is needed via different key

markState :: TaskState -> [Task] -> Maybe Int -> Int -> [Task]
markState _ ts Nothing _ = ts
markState st ts (Just i) time =
  case elemAtIndex ts i of
    Nothing -> ts
    (Just t) -> replaceElemAtIndexIfExists ts i (t {state = st, lastTick = time})

deleteListIndexSafe :: [Task] -> Maybe Int -> [Task]
deleteListIndexSafe ts Nothing = ts
deleteListIndexSafe ts (Just i) =
  case elemAtIndex ts i of
    Nothing -> ts
    (Just _) -> removeElemAtIndexSafe ts i

view :: Model -> IO Msg
view model = do
  case screen model of
    NormalScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "Tasks:"
      let tasksWithCursor = addCursor (tasks model) (index model)
      render tasksWithCursor (compareTick model)
      putStrLn ""
      putStrLn "Keys: select (up, down), add (a), update_time (u), done (d), todo (t), start/stop (s), delete (Del), quit (q)."
      putStrLn ""
      putStrLn ("Current model is: " ++ show model)
      return (CommandMsg Nop)
    AddTaskScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "New task: "
      text <- getLine
      return (CommandMsg (AddTask text))

render :: [Task] -> Int -> IO ()
render ts time = do
  mapM_ (printTask time) ts

printTask :: Int -> Task -> IO ()
printTask time task =
  if active task
    then
      putStrLn (showNew ++ ">" ++ showState ++ title task ++ showTick)
    else
      putStrLn (showNew ++ " " ++ showState ++ title task ++ showTick)
  where
    showTick = " (" ++ show (lastTick task) ++ ")"
    showNew = if lastTick task >= time then " * " else "   "
    showStateAux =
      case state task of
        Todo -> "[ ]"
        Started -> "[S]"
        Stopped -> "[T]"
        Done -> "[X]"
    showState = " " ++ showStateAux ++ " "

addCursor :: [Task] -> Maybe Int -> [Task]
addCursor ts Nothing = ts
addCursor ts (Just i) =
  case elemAtIndex ts i of
    Nothing -> ts
    Just t -> replaceElemAtIndexIfExists ts i (t {active = True})

replaceElemAtIndexIfExists :: [a] -> Int -> a -> [a]
replaceElemAtIndexIfExists ts i t =
  case elemAtIndex ts i of
    Just _ -> take i ts ++ [t] ++ drop (i + 1) ts
    Nothing -> ts

removeElemAtIndexSafe :: [a] -> Int -> [a]
removeElemAtIndexSafe ts i =
  case elemAtIndex ts i of
    Just _ -> take i ts ++ drop (i + 1) ts
    Nothing -> ts

elemAtIndex :: [a] -> Int -> Maybe a
elemAtIndex [] _ = Nothing
elemAtIndex ts i =
  if length ts > i && i >= 0
    then Just (ts !! i)
    else Nothing

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
        '3' -> return KeyDelete
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
