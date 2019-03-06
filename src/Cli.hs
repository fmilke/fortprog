
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

main :: IO ()
main = putStrLn greeting >> inputListener

inputListener :: IO ()
inputListener = do
  putStr "> "
  inp <- getLine
  case handleInput inp of
    Just io -> io >> inputListener
    Nothing -> putStrLn quitMsg

handleInput :: String -> Maybe (IO ())
handleInput inp
  | inp `elem` [":h", ":help"] = Just (putStrLn helpMsg)
  | inp `elem` [":q", ":quit"] = Nothing
  | inp == ""                  = Just (return ())
  | otherwise                  = Just (putStrLn invalidInputMsg)