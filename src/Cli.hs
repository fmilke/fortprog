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
data CliState = CliState (Maybe Prog, Strategy)

-- entry point for cli
main :: IO ()
main = putStrLn greeting >> inputListener (CliState (Nothing, defaultStrategy))

-- listens for input and delegates
-- input to handler; does this in a 
-- loop until handler returns Nothing
inputListener :: CliState -> IO ()
inputListener state = do
  putStr "> "
  inp <- getLine
  effect <- handleInput inp state
  case effect of
    Just (io, newState) -> io >> inputListener newState
    Nothing             -> putStrLn quitMsg

type InputEffect = Maybe ((IO ()), CliState)
type InputResult = IO (InputEffect)

handleInput :: String -> CliState -> InputResult
handleInput inp state = case toArgs inp of
  Just args -> handleCliCommand args state
  -- try to parse input as script input
  Nothing   -> return (Just (putStrLn "invalid arguments given", state))

type Args = [String]

handleCliCommand :: Args -> CliState -> InputResult
handleCliCommand [] state = return (Just (return (), state))
handleCliCommand (command:params) state
  | command `elem` [":h", ":help"] = return (Just (putStrLn helpMsg, state))
  | command `elem` [":q", ":quit"] = return (Nothing)
  | command `elem` [":s", ":set" ] = return (changeStrategy (head params) state)
  | command `elem` [":l", ":load"]
    && params == []                = handleLoad [] state
  | command `elem` [":l", ":load"] = handleLoad (head params) state
  | command `elem` [":p", ":prog"] = handleShowProg state
  | otherwise                      = return (Just (return (), state))

handleShowProg :: CliState -> InputResult
handleShowProg (CliState (Just prog, strat)) = return (Just (putStrLn (pretty prog), CliState (Just prog, strat)))
handleShowProg state = return (Just (putStrLn "No program loaded.", state))

-- if a filepath is given, loads the specified
-- program; otherwise unloads the current programm
handleLoad :: FilePath -> CliState -> InputResult
handleLoad []       (CliState (_, strat)) = return (Just (putStrLn "Unloaded module.", CliState (Nothing, strat)))
handleLoad filePath (CliState (_, strat)) = do
  parsed <- parseFile filePath
  case parsed of
    Left errMsg -> return (Just (putStrLn errMsg, CliState (Nothing, strat)))
    Right prog  -> return (Just (putStrLn "Module loaded.", CliState (Just prog, strat)))

changeStrategy :: String -> CliState -> InputEffect
changeStrategy [] state = Just (return (), state)
changeStrategy alias (CliState (prog, oldStrat)) = case getStrategyByAlias alias of
  Just newStrat -> Just (putStrLn ("Changed strategy to '" ++ alias ++ "'"), CliState (prog, newStrat))
  Nothing       -> Just (putStrLn "Invalid strategy alias, see :help for details", CliState (prog, oldStrat))

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