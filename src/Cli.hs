import Parser()
import Prog
import Strategy

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

invalidInputMsg :: String
invalidInputMsg = "Invalid input. Try :help for a list of commands"

quitMsg :: String
quitMsg = "Leaving..."

greeting :: String
greeting = "Welcome to Simple Haskell! \n\
  \Type \":help\" for help."

defaultStrategy :: Strategy
defaultStrategy = liStrategy
data CliState = CliState (Maybe Prog, Strategy)

main :: IO ()
main = putStrLn greeting >> inputListener (CliState (Nothing, defaultStrategy))

inputListener :: CliState -> IO ()
inputListener state = do
  putStr "> "
  inp <- getLine
  case handleInput inp state of
    Just (io, newState) -> io >> inputListener newState
    Nothing             -> putStrLn quitMsg

type InputResult = Maybe ((IO ()), CliState)

handleInput :: String -> CliState -> InputResult
handleInput inp state = case toArgs inp of
  Just args -> handleCliCommand args state
  -- try to parse input as script input
  Nothing   -> Just (putStrLn "invalid arguments given", state)

type Args = [String]

handleCliCommand :: Args -> CliState -> InputResult
handleCliCommand [] state = Just (return (), state)
handleCliCommand (command:params) state
  | command `elem` [":h", ":help"] = Just (putStrLn helpMsg, state)
  | command `elem` [":q", ":quit"] = Nothing
  | command `elem` [":s", ":set"]  = changeStrategy (head params) state
  | otherwise                      = Just (return (), state)

changeStrategy :: String -> CliState -> InputResult
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
    ":r", ":reload"
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