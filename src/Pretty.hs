module Pretty where

import Data.List(intercalate)
import Term
import Prog

-- Class instance for
-- displaying anything in pretty
class Pretty a where
  pretty :: a -> String

instance Pretty (Term) where
  pretty (Var n)     = n
  pretty (Comb n []) = n
  pretty (Comb n ts) = n ++ " " ++  (intercalate " " (map (\t -> pretty' t) ts))
    where
      pretty' (Var m)     = m
      pretty' (Comb m []) = m
      pretty' (Comb m xs) = "(" ++ m ++ " " ++  (intercalate " " (map (\t -> pretty' t) xs)) ++ ")"

instance Pretty (Prog) where
  pretty (Prog [])     = ""
  pretty (Prog [r])    = pretty r
  pretty (Prog (r:rs)) = (pretty r) ++ ('\n' : (pretty (Prog rs)))

instance Pretty (Rule) where
  pretty (Rule l r) = (pretty l) ++ " = " ++ (pretty r)