{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}

module Main (main, readKey) where

import Data.List (isPrefixOf)
import Data.Stack (Stack, stackNew, stackPop, stackPush, stackSize)
import GHC.IO.Handle (BufferMode (LineBuffering, NoBuffering), hSetBuffering)
import GHC.IO.Handle.FD (stdin, stdout)
import System.Console.ANSI
  ( Color (Black, Green, Magenta, Red, White),
    ColorIntensity (Dull, Vivid),
    ConsoleLayer (Background, Foreground),
    SGR (Reset, SetColor, SetSwapForegroundBackground),
    clearScreen,
    hNowSupportsANSI,
    hideCursor,
    setCursorPosition,
    setSGR,
    showCursor,
  )
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

-- TODO task details in a different place

data TaskState
  = Todo
  | Started
  | Stopped
  | Done
  deriving (Show, Eq, Read)

newtype TimeTick = TimeTick Int deriving (Show, Read, Eq, Ord)

data Task
  = Task
  { title :: String,
    active :: Bool,
    lastTick :: TimeTick,
    state :: TaskState,
    marked :: Bool
  }
  deriving (Show, Read, Eq)

data Screen
  = NormalScreen
  | AddTaskScreen
  | EditTaskScreen String
  | HelpScreen
  deriving (Show, Eq, Read)

data Model
  = Model
  { tick :: TimeTick,
    index :: Maybe Int,
    logs :: [String],
    tasks :: [Task],
    screen :: Screen,
    compareTick :: TimeTick,
    undoStack :: Stack [Task]
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
  | KeyF8
  | Key Char
  | KeyUnknown InputMode Char
  deriving (Show, Eq)

dataFile :: FilePath
dataFile = "taska.txt"

data Command
  = Nop
  | AddTask String
  | EditTask (Maybe Int) String
  | ToNormalMode
  deriving (Show, Eq)

data Msg = KeyMsg Key | CommandMsg Command deriving (Show, Eq)

initModel :: Model
initModel =
  Model
    { tick = TimeTick 0,
      index = Nothing,
      logs = [],
      tasks = [],
      screen = NormalScreen,
      compareTick = TimeTick 0,
      undoStack = stackNew
    }

main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if not stdoutSupportsANSI
    then
      putStrLn "Standard output does not support 'ANSI' escape codes."
    else do
      -- ignore all errors and start a fresh database
      databaseStr <- catchIOError (readFile dataFile) (\_ -> return (show initModel))
      let maybeModel = readMaybe databaseStr :: Maybe Model
      case maybeModel of
        Nothing ->
          putStrLn
            ( "Corrupt database file. Delete "
                ++ dataFile
                ++ " to start a new database on model change."
            )
        Just model -> loop (model {logs = []}) -- logs are kept only for debugging with the database file

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
          showCursor
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
persistModel model = do
  let cleanModel = model {undoStack = stackNew}
  writeFile dataFile (show cleanModel)

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
        Key 'h' -> model {screen = HelpScreen}
        Key 'e' ->
          case index model of
            Just i ->
              case elemAtIndex (tasks model) i of
                Just task -> model {screen = EditTaskScreen (title task)}
                Nothing -> model
            Nothing -> model
        Key 'c' ->
          model
            { compareTick = tick model,
              undoStack = stackPush (undoStack model) (tasks model)
            }
        Key 'U' ->
          let undoItem = stackPop (undoStack model)
           in case undoItem of
                Just (newUndoStack, newTasks) ->
                  model {tasks = newTasks, undoStack = newUndoStack}
                Nothing -> model
        Key ' ' ->
          model
            { tasks = markDone (tasks model) (index model) (tick model),
              tick = nextTick (tick model),
              undoStack = stackPush (undoStack model) (tasks model)
            }
        Key 't' ->
          model
            { tasks = markTodo (tasks model) (index model) (tick model),
              tick = nextTick (tick model),
              undoStack = stackPush (undoStack model) (tasks model)
            }
        Key 's' ->
          model
            { tasks = markStartStop (tasks model) (index model) (tick model),
              tick = nextTick (tick model),
              undoStack = stackPush (undoStack model) (tasks model)
            }
        KeyDelete ->
          model
            { tasks = newTasks,
              tick = nextTick (tick model),
              index = if not (null newTasks) then Just 0 else Nothing,
              undoStack = stackPush (undoStack model) (tasks model)
            }
          where
            newTasks = deleteListIndexSafe (tasks model) (index model)
        KeyF8 ->
          model
            { tick = nextTick (tick model),
              tasks = cleanUp (compareTick model) (tasks model),
              undoStack = stackPush (undoStack model) (tasks model)
            }
        Key 'm' ->
          model
            { tasks = markTask (tasks model) (index model) (tick model),
              tick = nextTick (tick model),
              undoStack = stackPush (undoStack model) (tasks model)
            }
        Key '+' ->
          model
            { tasks = moveTask pred (tasks model) (index model),
              index = newIndex,
              tick = nextTick (tick model)
            }
          where
            newIndex =
              fmap
                (\x -> max 0 (x - 1))
                (index model)
        Key '-' ->
          model
            { tasks = moveTask succ (tasks model) (index model),
              index = newIndex,
              tick = nextTick (tick model)
            }
          where
            newIndex =
              fmap
                (\x -> min (length (tasks model) - 1) (x + 1))
                (index model)
        Key c ->
          model {logs = ("No such command: " ++ show c) : logs model}
        KeyUnknown mode c -> model {logs = newLog : logs model}
          where
            newLog = "Unknown character " ++ show c ++ " in " ++ show mode ++ " mode."
        _ -> model
    CommandMsg command ->
      case command of
        AddTask s ->
          model
            { tasks = tasks model ++ [Task s False (tick model) Todo False],
              screen = NormalScreen,
              tick = nextTick (tick model),
              index = Just 0,
              undoStack = stackPush (undoStack model) (tasks model)
            }
        EditTask Nothing _ -> model {screen = NormalScreen}
        EditTask (Just _) [] -> model {screen = NormalScreen}
        EditTask (Just i) s ->
          model
            { tasks = case elemAtIndex (tasks model) i of
                Just t ->
                  replaceElemAtIndexIfExists
                    (tasks model)
                    i
                    (t {title = s, lastTick = tick model})
                Nothing -> tasks model,
              screen = NormalScreen,
              tick = newTick,
              index = Just 0,
              undoStack = stackPush (undoStack model) (tasks model)
            }
          where
            newTick = nextTick (tick model)
        ToNormalMode ->
          model {screen = NormalScreen}
        Nop -> model

moveTask :: (Int -> Int) -> [a] -> Maybe Int -> [a]
moveTask _ ts Nothing = ts
moveTask f ts (Just i) =
  case theItem of
    Nothing -> ts
    Just _ -> case otherItem of
      Nothing -> ts
      Just _ -> swapExistingIndexes ts i (f i)
  where
    theItem = elemAtIndex ts i
    otherItem = elemAtIndex ts (f i)

swapExistingIndexes :: [a] -> Int -> Int -> [a]
swapExistingIndexes xs i1 i2 =
  replaceElemAtIndexIfExists xs1 i2 elemAtIndex1
  where
    elemAtIndex1 = xs !! i1
    elemAtIndex2 = xs !! i2
    xs1 = replaceElemAtIndexIfExists xs i1 elemAtIndex2

cleanUp :: TimeTick -> [Task] -> [Task]
cleanUp time = filter (not . deletableTask time)

deletableTask :: TimeTick -> Task -> Bool
deletableTask time task =
  (lastTick task < time) && (state task == Done)

markDone :: [Task] -> Maybe Int -> TimeTick -> [Task]
markDone = markState Done

markTodo :: [Task] -> Maybe Int -> TimeTick -> [Task]
markTodo = markState Todo

markStartStop :: [Task] -> Maybe Int -> TimeTick -> [Task]
markStartStop ts Nothing _ = ts
markStartStop ts (Just i) time =
  case elemAtIndex ts i of
    Nothing -> ts
    (Just t) ->
      replaceElemAtIndexIfExists
        ts
        i
        (t {state = newState (state t), lastTick = time})
  where
    newState Todo = Started
    newState Started = Stopped
    newState Stopped = Started
    newState Done = Done -- explicit Todo state is needed via different key

markTask :: [Task] -> Maybe Int -> TimeTick -> [Task]
markTask ts Nothing _ = ts
markTask ts (Just i) time =
  case elemAtIndex ts i of
    Nothing -> ts
    (Just t) ->
      replaceElemAtIndexIfExists
        ts
        i
        (t {marked = not (marked t), lastTick = time})

markState :: TaskState -> [Task] -> Maybe Int -> TimeTick -> [Task]
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
  showCursor
  case screen model of
    NormalScreen -> do
      hideCursor
      clearScreen
      setCursorPosition 0 0
      let undoStackSize = stackSize (undoStack model)
      putStrLn $ "Tasks:           [press 'h' for help, Undo: " ++ show undoStackSize ++ "]"
      let tasksWithCursor = addCursor (tasks model) (index model)
      render tasksWithCursor (compareTick model)
      return (CommandMsg Nop)
    AddTaskScreen -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "New task: "
      text <- getLine
      return (CommandMsg (AddTask text))
    EditTaskScreen originalTitle -> do
      clearScreen
      setCursorPosition 0 0
      putStrLn "Original title was: "
      setSGR [SetSwapForegroundBackground True]
      putStrLn originalTitle
      setSGR [Reset]
      putStrLn ""
      putStrLn "Edited title (leave empty to cancel): "
      text <- getLine
      return (CommandMsg (EditTask (index model) text))
    HelpScreen -> do
      hideCursor
      clearScreen
      setCursorPosition 0 0
      putStrLn "Help screen"
      putStrLn ""
      renderKeyHelper keys
      putStrLn ""
      putStrLn "Press ENTER to exit"
      _ <- getLine
      return (CommandMsg ToNormalMode)

keys :: [(String, String)]
keys =
  [ ("a", "add task"),
    ("c", "checkpoint in time"),
    ("d", "mark task as done"),
    ("Del", "delete task"),
    ("e", "edit task title"),
    ("m", "mark"),
    ("q", "quit"),
    ("s", "start/stop task"),
    ("t", "mark task as todo"),
    ("U", "undo"),
    ("Up/Down", "move in list"),
    ("F8", "clean up done tasks"),
    ("-", "move task down"),
    ("+", "move task up")
  ]

renderKeyHelper :: [(String, String)] -> IO ()
renderKeyHelper = mapM_ renderKey

renderKey :: (String, String) -> IO ()
renderKey (k, s) = putStrLn $ k ++ ": " ++ s

render :: [Task] -> TimeTick -> IO ()
render ts time = do
  mapM_ (renderTask time) ts

renderTask :: TimeTick -> Task -> IO ()
renderTask time task
  | active task = do
      setSGR [SetSwapForegroundBackground True]
      renderTaskLine task
      setSGR [Reset]
  | lastTick task >= time = do
      setSGR [SetColor Background Dull Green]
      setSGR [SetColor Foreground Dull Black]
      renderTaskLine task
      setSGR [Reset]
  | otherwise = do
      setSGR [SetColor Foreground Dull White]
      renderTaskLine task
      setSGR [Reset]

renderTaskLine :: Task -> IO ()
renderTaskLine task = do
  putStr cursor
  putStr showState
  putStr titleNoTags
  setSGR [SetColor Foreground Dull Red]
  putStr tagsPart
  setSGR [SetColor Background Vivid Magenta]
  putStrLn mark
  where
    showStateAux =
      case state task of
        Todo -> "[ ]"
        Started -> "[S]"
        Stopped -> "[T]"
        Done -> "[X]"
    showState = " " ++ showStateAux ++ " "
    mark = if marked task then "  " else ""
    titleTags = filter (isPrefixOf "#") (words (title task))
    tagsPart = if null titleTags then "" else " [" ++ unwords titleTags ++ "]"
    titleNoTags = unwords (filter (not . isPrefixOf "#") (words (title task)))
    cursor = if active task then ">" else " "

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
        '1' -> return KeyF8
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

nextTick :: TimeTick -> TimeTick
nextTick (TimeTick i) = TimeTick (i + 1)