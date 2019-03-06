import Parser
import Pretty
import Prog

test :: IO ()
test = do
  res <- (parseFile "../examples/Peano.hs") :: IO (Either String Prog)
  putStrLn (asd res)

asd :: (Either String Prog) -> String
asd res = case res of
  Left s  -> s
  Right s -> (pretty s)