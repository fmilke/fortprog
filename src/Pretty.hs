module Pretty(
	Pretty
) where

import Term

-- Class instance for
-- displaying anything in pretty
class Pretty a where
	pretty :: a -> String


instance Pretty (Term) where
	pretty (Var n) = n
	pretty (Comb n []) = n
	pretty (Comb n ts) = n ++ " (" ++ (foldr (\t rest -> (pretty t) ++ " " ++ rest) "" ts) ++ ")"