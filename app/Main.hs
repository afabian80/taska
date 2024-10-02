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
    logs :: [String]
  }
  deriving (Show, Read)

-- data Msg = Increment | Decrement | NormalMode | AddMode | Quit | Nop deriving (Show, Eq)

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
  | Key Char
  | KeyUnknown InputMode Char
  deriving (Show, Eq)

dataFile :: FilePath
dataFile = "taska.txt"

initModel :: Model
initModel =
  Model
    { tick = 0,
      counter = 0,
      logs = []
    }

main :: IO ()
main = do
  databaseStr <- catchIOError (readFile dataFile) (\_ -> return (show initModel)) -- ignore all errors and start a fresh database
  let maybeModel = readMaybe databaseStr :: Maybe Model
  case maybeModel of
    Nothing -> putStrLn "Corrupt database file."
    Just model -> do
      let newModel = model {logs = []} -- clean logs when loading model database
      loop newModel

loop :: Model -> IO ()
loop model = do
  view model
  hSetBuffering stdin NoBuffering -- so pressing Enter is not needed after character input
  key <- readKey IMNormal
  hSetBuffering stdin LineBuffering -- turn back the need for Enter and line buffering
  if key == Key 'q'
    then do
      persistModel model
      putStrLn "Bye"
    else do
      let newModel = update key model
      persistModel newModel
      loop newModel

persistModel :: Model -> IO ()
persistModel model =
  writeFile dataFile (show model)

update :: Key -> Model -> Model
update key model =
  case key of
    KeyUp ->
      model
        { counter = counter model + 1,
          tick = tick model + 1
        }
    KeyDown ->
      model
        { counter = counter model - 1,
          tick = tick model + 1
        }
    KeyUnknown mode c -> model {logs = newLog : logs model}
      where
        newLog = "Unknown character " ++ show c ++ " in " ++ show mode ++ " mode."
    _ -> model

view :: Model -> IO ()
view model = do
  clearScreen
  setCursorPosition 0 0
  putStrLn ("Current model is: " ++ show model)
  putStrLn ""
  putStrLn ""
  putStrLn "'Up arrow' to increment, Down arrow' to decrement, 'q' to quit."

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
        _ -> return (KeyUnknown IMAwaitBracket c)
    IMAwaitControl -> do
      c <- getChar
      case c of
        'D' -> return KeyLeft
        'C' -> return KeyRight
        'A' -> return KeyUp
        'B' -> return KeyDown
        'H' -> return KeyHome
        'F' -> return KeyEnd
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
