module Parser
  ( Parse(..), parseFile
  ) where

import System.IO.Error (catchIOError)

import Text.Parsec hiding (parse)

import Prog
import Term

-- Type class for parsing.
class Parse a where
  parse :: String -> Either String a
  
instance Parse Prog where
  parse = simpleParse (prog <* eof)

instance Parse Term where
  parse = either (Left . ("Parse error (" ++) . drop 21) Right .
    simpleParse (expr False <* eof)

-- Parses the content of a file.
parseFile :: Parse a => FilePath -> IO (Either String a)
parseFile file = catchIOError (readFile file >>= return . parse)
  (const (return (Left "Could not read file.")))

-- INTERNAL STUFF

type Parser a =
  Parsec String ([String], [String], [String], [(String, (Int, Int))]) a

simpleParse :: Parser a -> String -> Either String a
simpleParse parser = either (Left . ("Parse error " ++) . show) Right .
  runParser parser ([], [], [], []) ""

fst4 :: (a, b, c, d) -> a
fst4 (w, _, _, _) = w

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

trd4 :: (a, b, c, d) -> c
trd4 (_, _, y, _) = y

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, z) = z

resetVars :: Parser ()
resetVars = getState >>=
  \ (_, tyCons, cons, funMap) -> putState ([], tyCons, cons, funMap)

getVars :: Parser [String]
getVars = fst4 <$> getState

addVar :: VarName -> Parser ()
addVar name = getState >>=
  \ (vars, tyCons, cons, funMap) -> putState (name : vars, tyCons, cons, funMap)

getTyCons :: Parser [String]
getTyCons = snd4 <$> getState

addTyCon :: String -> Parser ()
addTyCon name = getState >>=
  \ (vars, tyCons, cons, funMap) -> putState (vars, name : tyCons, cons, funMap)

getCons :: Parser [String]
getCons = trd4 <$> getState

addCon :: String -> Parser ()
addCon name = getState >>=
  \ (vars, tyCons, cons, funMap) ->
    putState (vars, tyCons, name : cons, funMap)

getFunMap :: Parser [(String, (Int, Int))]
getFunMap = fth4 <$> getState

addFun :: String -> (Int, Int) -> Parser ()
addFun name pat = getState >>=
  \ (vars, tyCons, cons, funMap) ->
    putState (vars, tyCons, cons, (name, pat) : funMap)

prog :: Parser Prog
prog =
  whitespaces *> skipMany newlines *> modHeader *> (Prog . concat <$> many decl)

modHeader :: Parser ()
modHeader = optional (try (keyword "module" *> modName <* whitespaces <*
  keyword "where" <* newlines)) <?> "module header"

decl :: Parser [Rule]
decl = resetVars >> (try dataDecl <|> try sig <|> try rule) <* newlines

dataDecl :: Parser [a]
dataDecl = (do
  keyword "data"
  name <- tyConName
  knownTyCons <- getTyCons
  if name `elem` knownTyCons
    then unexpected ("redefined data type `" ++ name ++ "`")
    else addTyCon name
  whitespaces
  skipMany tyVarPat
  symbol "="
  _ <- conDecl `sepBy1` symbol "|"
  return []) <?> "data type declaration"

tyVarPat :: Parser ()
tyVarPat = (do
  name <- tyVarName
  case name of
    "_" -> whitespaces
    _   -> do
      knownVars <- getVars
      if name `elem` knownVars
        then unexpected ("repeated type variable `" ++ name ++ "`")
        else do
          addVar name
          whitespaces) <?> "type variable"

conDecl :: Parser ()
conDecl = (do
  name <- conName
  knownCons <- getCons
  if name `elem` knownCons
    then unexpected ("redefined constructor `" ++ name ++ "`")
    else addCon name
  whitespaces
  skipMany (tyExpr True)
  return ()) <?> "constructor declaration"

tyExpr :: Bool -> Parser ()
tyExpr False = tyVar <|>
  parens True (() <$ tyExpr False `sepBy1` symbol "->") <|>
    tyConName *> whitespaces *> skipMany (tyExpr True) <?> "type expression"
tyExpr True  = tyVar <|>
  try (parens True (tyConName *> whitespaces *> skipMany1 (tyExpr True))) <|>
    parens True (() <$ tyExpr False `sepBy1` symbol "->") <|>
      tyConName *> whitespaces <?> "type expression"

tyVar :: Parser ()
tyVar = (do
  name <- tyVarName
  case name of
    "_" -> unexpected "anonymous type variable"
    _   -> do
      knownVars <- getVars
      if name `notElem` knownVars
        then unexpected ("unknown type variable `" ++ name ++ "`")
        else whitespaces) <?> "type variable"

sig :: Parser [a]
sig = (do
  _ <- funName
  whitespaces
  symbol "::"
  _ <- sigTyExpr False `sepBy1` symbol "->"
  return []) <?> "type signature"

sigTyExpr :: Bool -> Parser ()
sigTyExpr False = sigTyVar <|>
  parens True (() <$ sigTyExpr False `sepBy1` symbol "->") <|>
    tyConName *> whitespaces *> skipMany (sigTyExpr True) <?> "type expression"
sigTyExpr True  = sigTyVar <|>
  try (parens True (tyConName *> whitespaces *> skipMany1 (sigTyExpr True))) <|>
    parens True (() <$ sigTyExpr False `sepBy1` symbol "->") <|>
      tyConName *> whitespaces <?> "type expression"
  
sigTyVar :: Parser ()
sigTyVar = (do
  name <- tyVarName
  case name of
    "_" -> unexpected "anonymous type variable"
    _   -> whitespaces) <?> "type variable"

rule :: Parser [Rule]
rule = return <$> (Rule <$> lhs <* symbol "=" <*> rhs) <?> "function rule"

lhs :: Parser Lhs
lhs = do
  name <- funName
  whitespaces
  funMap <- getFunMap
  case lookup name funMap of
    Nothing                             -> do
      varPats <- many varPat
      conAndVarPats <- (:) <$> conPat <*> many varPat <|> return []
      addFun name (length varPats, length conAndVarPats)
      return (Comb name (varPats ++ conAndVarPats))
    Just (numVarPats, numConAndVarPats) -> do
      varPats <- count numVarPats varPat
      conAndVarPats <-
        if numConAndVarPats > 0
          then (:) <$> conPat <*> count (numConAndVarPats - 1) varPat
          else return []
      return (Comb name (varPats ++ conAndVarPats))

conPat :: Parser Term
conPat = parens True (Comb <$> conName <* whitespaces <*> many1 varPat) <|>
  flip Comb [] <$> conName <* whitespaces <?> "constructor pattern"

varPat :: Parser Term
varPat = (do
  name <- varName
  case name of
    "_" -> Var name <$ whitespaces
    _   -> do
      knownVars <- getVars
      if name `elem` knownVars
        then unexpected ("repeated variable `" ++ name ++ "`")
        else do
          addVar name
          Var name <$ whitespaces) <?> "variable pattern"

rhs :: Parser Rhs
rhs = expr False <|> expr True

expr :: Bool -> Parser Term
expr False = try var <|>
  Comb <$> (conName <|> funName) <* whitespaces <*> many (expr True) <|>
    expr True <?> "expression"
expr True  = try var <|>
  parens True (Comb <$> (conName <|> funName) <* whitespaces <*>
                many1 (expr True)) <|>
    flip Comb [] <$> (conName <|> funName) <* whitespaces <?> "expression"

var :: Parser Term
var = (do
  name <- varName
  case name of
    "_" -> unexpected "anonymous variable"
    _   -> do
      knownVars <- getVars
      if name `elem` knownVars
        then Var name <$ whitespaces
        else fail "") <?> "variable"

modName :: Parser String
modName = nameStartingWith upper <?> "module name"

tyVarName :: Parser String
tyVarName = varName <?> "type variable name"

tyConName :: Parser String
tyConName = nameStartingWith upper <?> "type constructor name"

varName :: Parser String
varName = nameStartingWith (lower <|> char '_') <?> "variable name"

conName :: Parser String
conName = nameStartingWith upper <?> "constructor name"

funName :: Parser String
funName = (do
  name <- nameStartingWith lower
  case name of
    "data"   -> unexpected "keyword `data`"
    "module" -> unexpected "keyword `module`"
    _        -> do
      knownVars <- getVars
      if name `elem` knownVars
        then unexpected ("variable `" ++ name ++ "`")
        else return name) <?> "function name"

nameStartingWith :: Parser Char -> Parser String
nameStartingWith c =
  (:) <$> c <*> many (letter <|> digit <|> oneOf "_'" <?> "")

symbol :: String -> Parser ()
symbol str = string str *> whitespaces

keyword :: String -> Parser ()
keyword str = string str *> whitespaces

comment :: Parser ()
comment = singleLineComment <|> multiLineComment <?> ""

singleLineComment :: Parser ()
singleLineComment = try
  (() <$ (string "--" *> manyTill anyChar (lookAhead (() <$ newline <|> eof))))

multiLineComment :: Parser ()
multiLineComment =
  () <$ (string "{-" *> manyTill anyChar (try (string "-}")))

newlines :: Parser ()
newlines = skipMany1 (newline <* whitespaces <?> "newline")

whitespaces :: Parser ()
whitespaces = skipMany (() <$ (oneOf " \t" <?> "") <|> comment)

parens :: Bool -> Parser a -> Parser a
parens False = id
parens True  = (<?> "") . between (symbol "(") (symbol ")")
