import Parser(parseFile)
import Prog
import Strategy
import Pretty

helpMsg :: String
helpMsg = "Commands available from the prompt:\n\
  \  <expression>       Evaluates the specified expression.\n\
  \  :h[elp]            Shows this help message.\n\
  \  :l[oad] <file>     Loads the specified file.\n\
  \  :l[oad]            Unloads the currently loaded file.\n\
  \  :r[eload]          Reloads the lastly loaded file.\n\
  \  :s[et] <strategy>  Sets the specified evaluation strategy\n\
  \  :p[rog]            Outputs the currently loaded program\n\
  \                     where <strategy> is one of 'lo', 'li',\n\
  \                     'ro', 'ri', 'po', or 'pi'.\n\
  \  :q[uit]            Exits the interactive environment."

quitMsg :: String
quitMsg = "Leaving..."

greeting :: String
greeting = "Welcome to Simple Haskell! \n\
  \Type \":help\" for help."

defaultStrategy :: Strategy
defaultStrategy = liStrategy

type HaltProgram = Bool
type LastFilePath = FilePath
data CliState = CliState (Maybe Prog, Strategy, HaltProgram, LastFilePath)

initialState :: CliState
initialState = CliState (Nothing, defaultStrategy, False, "")

setStrategy :: CliState -> Strategy -> CliState
setStrategy (CliState (p, _, h, fp)) s = CliState (p, s, h, fp)

setHaltFlag :: CliState -> HaltProgram -> CliState
setHaltFlag (CliState (p, s, _, fp)) h = CliState (p, s, h, fp)

setProgram :: CliState -> Maybe Prog -> CliState
setProgram (CliState (_, s, h, fp)) p = CliState (p, s, h, fp)

getProgram :: CliState -> Maybe Prog
getProgram (CliState (p, _, _, _)) = p

-- entry point for cli
main :: IO ()
main = putStrLn greeting >> inputListener initialState

-- listens for input and delegates
-- input to handler; does this in a 
-- loop until handler returns Nothing
inputListener :: CliState -> IO ()
inputListener state = do
  putStr "> "
  inp <- getLine
  effect <- handleInput inp state
  case effect of
    (CliState (_, _, True, _)) -> putStrLn quitMsg
    newState                   -> inputListener newState

type InputResult = IO (CliState)

handleInput :: String -> CliState -> InputResult
handleInput inp state = case toArgs inp of
  Just args -> handleCliCommand args state
  -- TODO: try to parse input as script input
  Nothing   -> (putStrLn "invalid arguments given") >> return (state)

type Args = [String]

handleCliCommand :: Args -> CliState -> InputResult
handleCliCommand [] state = return (state)
handleCliCommand (command:params) state
  | command `elem` [":h", ":help"] = (putStrLn helpMsg) >> return (state)
  | command `elem` [":q", ":quit"] = return (setHaltFlag state True)
  | command `elem` [":s", ":set" ] = handleStrategyChange (head params) state
  | command `elem` [":l", ":load"]
    && params == []                = handleLoad [] state
  | command `elem` [":l", ":load"] = handleLoad (head params) state
  | command `elem` [":p", ":prog"] = handleShowProg state
  | otherwise                      = return (state)

handleShowProg :: CliState -> InputResult
handleShowProg state = case getProgram state of 
  Nothing -> (putStrLn "No program loaded.") >> return (state)
  Just p  -> (putStrLn (pretty p)) >> return (state)

-- if a filepath is given, loads the specified
-- program; otherwise unloads the current programm
handleLoad :: FilePath -> CliState -> InputResult
handleLoad []       state = (putStrLn "Unloaded module.") >> return (setProgram state Nothing)
handleLoad filePath state = do
  parsed <- parseFile filePath
  case parsed of
    Left errMsg -> (putStrLn errMsg) >> return (state)
    Right prog  -> (putStrLn "Module loaded.") >> return (setProgram state (Just prog))

handleStrategyChange :: String -> CliState -> InputResult
handleStrategyChange [] state = return (state)
handleStrategyChange alias state = case getStrategyByAlias alias of
  Just newStrat -> (putStrLn ("Changed strategy to '" ++ alias ++ "'")) >> return (setStrategy state newStrat)
  Nothing       -> (putStrLn "Invalid strategy alias, see :help for details") >> return (state)

getStrategyByAlias :: String -> Maybe Strategy
getStrategyByAlias alias
  | alias == "lo" = Just loStrategy
  | alias == "li" = Just liStrategy
  | alias == "ro" = Just roStrategy
  | alias == "ri" = Just riStrategy
  | otherwise     = Nothing

-- a list of all valid commands
-- (except Simpel Haskel syntax)
validCommands :: [String]
validCommands = [
    ":q", ":quit",
    ":h", ":help",
    ":s", ":set",
    ":l", ":load",
    ":r", ":reload",
    ":p", ":prog" -- displays current program
  ]

isValidCommand :: String -> Bool
isValidCommand command =  command `elem` validCommands
  
-- checks if a string qualifies
-- for :command <param> syntax
-- and parses it if possible
toArgs :: String -> Maybe Args
toArgs str = toArgs' str [] []
  where
    toArgs' []       acc result = Just (result ++ [acc])
    toArgs' (' ':cs) acc []     = if isValidCommand acc
      then toArgs' cs [] [acc]
      else Nothing
    toArgs' (' ':cs) acc result = toArgs' cs [] (result ++ [acc])
    toArgs' (c  :cs) acc result = toArgs' cs (acc ++ [c]) result