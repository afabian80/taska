{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}

module Main (main, readKey) where

import Data.Stack ( Stack, stackNew, stackPop, stackPush, stackSize )
import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin, stdout)
import System.Console.ANSI
    ( clearScreen,
      hNowSupportsANSI,
      setCursorPosition,
      setSGR,
      Color(White, Green, Black),
      ColorIntensity(Dull),
      ConsoleLayer(Foreground, Background),
      SGR(Reset, SetSwapForegroundBackground, SetColor) )
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

-- TODO database gets huge very quickly (~10 tasks), because of the undo stack
-- TODO task details in a different place

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

data Screen
  = NormalScreen
  | AddTaskScreen
  | EditTaskScreen
  deriving (Show, Eq, Read)

data Model
  = Model
  { tick :: Int,
    index :: Maybe Int,
    logs :: [String],
    tasks :: [Task],
    screen :: Screen,
    compareTick :: Int,
    undoStack :: Stack Model
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

data Command
  = Nop
  | AddTask String
  | EditTask (Maybe Int) String
  deriving (Show, Eq)

data Msg = KeyMsg Key | CommandMsg Command deriving (Show, Eq)

initModel :: Model
initModel =
  Model
    { tick = 0,
      index = Nothing,
      logs = [],
      tasks = [],
      screen = NormalScreen,
      compareTick = 0,
      undoStack = stackNew
    }

main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if not stdoutSupportsANSI
    then
      putStrLn "Standard output does not support 'ANSI' escape codes."
    else
      do
        databaseStr <- catchIOError (readFile dataFile) (\_ -> return (show initModel)) -- ignore all errors and start a fresh database
        let maybeModel = readMaybe databaseStr :: Maybe Model
        case maybeModel of
          Nothing -> putStrLn ("Corrupt database file. Delete " ++ dataFile ++ " to start a new database on model change.")
          Just model -> do
            let newModel = model {logs = [], undoStack = stackNew} -- clean logs and undo when loading model database
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
        Key 'e' ->
          case index model of
            Just _ -> model {screen = EditTaskScreen}
            Nothing -> model
        Key 'c' -> model {compareTick = tick model, undoStack = stackPush (undoStack model) model}
        Key 'U' ->
          let
            undoItem = stackPop (undoStack model)
          in
            case undoItem of
              Just (newUndoStack, newModel) ->
                newModel {compareTick = tick newModel, undoStack = newUndoStack}
              Nothing -> model
        Key ' ' ->
          model
            { tasks = markDone (tasks model) (index model) (tick model),
              tick = tick model + 1,
              undoStack = stackPush (undoStack model) model
            }
        Key 't' ->
          model
            { tasks = markTodo (tasks model) (index model) (tick model),
              tick = tick model + 1,
              undoStack = stackPush (undoStack model) model
            }
        Key 's' ->
          model
            { tasks = markStartStop (tasks model) (index model) (tick model),
              tick = tick model + 1,
              undoStack = stackPush (undoStack model) model
            }
        KeyDelete ->
          model
            { tasks = newTasks,
              tick = tick model + 1,
              index = if not (null newTasks) then Just 0 else Nothing,
              undoStack = stackPush (undoStack model) model
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
              index = Just 0,
              undoStack = stackPush (undoStack model) model
            }
        EditTask Nothing _ -> model {screen = NormalScreen}
        EditTask (Just _) [] -> model {screen = NormalScreen}
        EditTask (Just i) s ->
          model
            { tasks = case elemAtIndex (tasks model) i of
                Just t -> replaceElemAtIndexIfExists (tasks model) i (t {title = s, lastTick = tick model})
                Nothing -> tasks model,
              screen = NormalScreen,
              tick = newTick,
              index = Just 0,
              undoStack = stackPush (undoStack model) model
            }
          where
            newTick = tick model + 1
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
      putStrLn "Keys: select (up, down), add (a), checkpoint (c), done (d), todo (t), start/stop (s), delete (Del), edit (e), undo (U), quit (q)."
      putStrLn ""
      let undoStackSize = stackSize ( undoStack model)
      putStrLn ("Undo stack size: " ++ show undoStackSize)
      -- let modelWithoutUndoStack = model {undoStack = stackNew}
      -- putStrLn ("Current model is: " ++ show modelWithoutUndoStack)
      return (CommandMsg Nop)
    AddTaskScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "New task: "
      text <- getLine
      return (CommandMsg (AddTask text))
    EditTaskScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "Edited title (leave empty to cancel): "
      text <- getLine
      return (CommandMsg (EditTask (index model) text))

render :: [Task] -> Int -> IO ()
render ts time = do
  mapM_ (renderTask time) ts

renderTask :: Int -> Task -> IO ()
renderTask time task
  | active task = do
      setSGR [SetSwapForegroundBackground True]
      putStrLn (">" ++ showState ++ title task)
      setSGR [Reset]
  | isNew = do
          setSGR [SetColor Background Dull Green]
          setSGR [SetColor Foreground Dull Black]
          putStrLn (" " ++ showState ++ title task)
          setSGR [Reset]
  | otherwise = do
          setSGR [SetColor Foreground Dull White]
          putStrLn (" " ++ showState ++ title task)
          setSGR [Reset]
  where
      isNew = lastTick task >= time
      showStateAux
        = case state task of
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
