module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser
import Jq.Json

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- token . char $ '('
  f <- parseConstructor
  _ <- token . char $ ')'
  return (Parenthesis f)

parseObjIndex :: Parser Filter
parseObjIndex = do
  p <- parseObjId <|> parseObjIdentId
  ps <- many (parseObjId <|> parseObjIdentId <|> parseNextIterator)
  return (makePipe (p : ps))

parseObjId :: Parser Filter
parseObjIdentId :: Parser Filter
parseObjIdentId = do
  _ <- token . char $ '.'
  s <- parseKeyQuotes <|> parseKeyNoQuotes
  isOpt <- token (char '?') <|> return ' '
  if isOpt == '?'
    then return (Optional (ObjIndex s))
    else return (ObjIndex s)
parseObjId = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  s <- parseString
  _ <- token . char $ ']'
  isOpt <- token (char '?') <|> return ' '
  if isOpt == '?'
    then return (Optional (ObjIndex s))
    else return (ObjIndex s)

parseArrIndex :: Parser Filter
parseArrIndex = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  idx <- int
  _ <- token . char $ ']'
  return (ArrIndex idx)

parseSlice :: Parser Filter
parseSlice = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  noFrom <- token (char ':') <|> return ' '
  if noFrom == ':'
  then do
    to <- parseConstructor
    _ <- token . char $ ']'
    return (Slice (Value JNull) to)
  else do
    from <- parseConstructor
    _ <- token . char $ ':'
    noTo <- token (char ']') <|> return ' '
    if noTo == ']'
    then return (Slice from (Value JNull))
    else do
      to <- parseConstructor
      _ <- token . char $ ']'
      return (Slice from to)

parseIterator :: Parser Filter
parseIterator = do
  i <- parseFirstIterator
  is <- many (parseNextIterator <|> parseObjIndex)
  return (makePipe (i : is))

parseFirstIterator :: Parser Filter
parseFirstIterator = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  isEmpty <- token (char ']') <|> return ' '
  if isEmpty == ']'
    then do
      isOpt <- token (char '?') <|> return ' '
      if isOpt == '?'
        then return (Optional (Iterator []))
        else return (Iterator [])
    else do
      f <- parseConstructor
      _ <- token . char $ ']'
      isOpt <- token (char '?') <|> return ' '
      if isOpt == '?'
        then return (Optional (Iterator [f]))
        else return (Iterator [f])

parseNextIterator :: Parser Filter
parseNextIterator = do
  _ <- token . char $ '['
  isEmpty <- token (char ']') <|> return ' '
  if isEmpty == ']'
    then do
      isOpt <- token (char '?') <|> return ' '
      if isOpt == '?'
        then return (Optional (Iterator []))
        else return (Iterator [])
    else do
      f <- parseConstructor
      _ <- token . char $ ']'
      isOpt <- token (char '?') <|> return ' '
      if isOpt == '?'
        then return (Optional (Iterator [f]))
        else return (Iterator [f])


parseDescent :: Parser Filter
parseDescent = do
  _ <- token . string $ ".."
  return Descent

parseComma :: Parser Filter
parseComma = do
  f <- parseFilter
  fs <- many (do _ <- token (char ',')
                 parseFilter)
  return $ makeComma (f : fs)

makeComma :: [Filter] -> Filter
makeComma [] = Identity
makeComma [f] = f
makeComma (f : fs) = Comma f (makeComma fs)

parsePipe :: Parser Filter
parsePipe = do
  f <- parseCompare <|> parseLogical <|> parseComma
  fs <- many (do _ <- token (char '|')
                 parseCompare <|> parseLogical <|> parseComma)
  return $ makePipe (f : fs)

makePipe :: [Filter] -> Filter
makePipe [] = Identity
makePipe [f] = f
makePipe (f : fs) = Pipe f (makePipe fs)

parseFilter :: Parser Filter
parseFilter =  do
  f <- parseParenthesis <|> parseValue <|> parseObjIndex <|>
       parseSlice <|> parseIterator <|> parseDescent <|> parseIdentity <|> parseNot
  isOpt <- token (char '?') <|> return ' '
  if isOpt == '?'
    then do
      _ <- many (token (char '?'))
      return (Optional f)
    else return f

parseFNull :: Parser Filter
parseFNull = do
  n <- parseJNull
  return (Value n)

parseFBool :: Parser Filter
parseFBool = do
  b <- parseJBool
  return (Value b)

parseFString :: Parser Filter
parseFString = do
  s <- parseJString
  return (Value s)

parseFNumber :: Parser Filter
parseFNumber = do
  n <- parseJFloat <|> parseJNumber
  return (Value n)

parseFArray :: Parser Filter
parseFArray = do
  _ <- token . char $ '['
  isEmpty <- token (char ']') <|> return ' '
  if isEmpty == ']'
    then return (Array DoNothing)
    else do
      f <- parseConstructor
      _ <- token . char $ ']'
      return (Array f)

parseFObject :: Parser Filter
parseFObject = do
  _ <- token . char $ '{'
  isEmpty <- token (char '}') <|> return ' '
  if isEmpty == '}'
    then return (Object [])
    else do
      p <- parsePair
      ps <- many (do _ <- token (char ',')
                     parsePair)
      _ <- token . char $ '}'
      return (Object (p : ps))
      where
        parsePair = parseKV <|> parseShort


parseShort :: Parser (Filter, Filter)
parseShort = do
  s <- parseKeyQuotes <|> parseKeyNoQuotes
  return (ObjectKey s, Optional (ObjIndex s))

parseKV :: Parser (Filter, Filter)
parseKV = do
  k <- parseKey <|> parseParenthesis
  _ <- token . char $ ':'
  v <- parseConstructor
  return (k, v)

parseKey :: Parser Filter
parseKey = do
  k <- parseKeyQuotes <|> parseKeyNoQuotes
  return (ObjectKey k)

parseKeyQuotes :: Parser String
parseKeyQuotes = do
  _ <- char '"'
  s <- some (escapeChar <|> sat (/= '"'))
  _ <- char '"'
  return s

parseKeyNoQuotes :: Parser String
parseKeyNoQuotes = do
  s <- letter <|> char '_'
  ss <- many (alphanum <|> char '_')
  return (s:ss)

parseValue :: Parser Filter
parseValue = do
  v <- parseFNull <|> parseFBool <|> parseFString <|> parseFNumber <|> parseFArray <|> parseFObject
  isOpt <- token (char '?') <|> return ' '
  if isOpt == '?'
    then return (Optional v)
    else return v

parseConstructor :: Parser Filter
parseConstructor = parseTryCatch <|> parseConditional <|> parsePipe

parseLogical :: Parser Filter
parseLogical = parseAnd <|> parseOr

parseAnd :: Parser Filter
parseAnd = do
  f <- parseFilter
  fs <- some (do _ <- token . string $ "and"
                 parseFilter)
  return $ makeAnd (f : fs)

makeAnd :: [Filter] -> Filter
makeAnd [] = Identity
makeAnd [f] = f
makeAnd (f : fs) = And f (makeAnd fs)

parseOr :: Parser Filter
parseOr = do
  f <- parseFilter
  fs <- some (do _ <- token . string $ "or"
                 parseFilter)
  return $ makeOr (f : fs)

makeOr :: [Filter] -> Filter
makeOr [] = Identity
makeOr [f] = f
makeOr (f : fs) = Or f (makeOr fs)

parseNot :: Parser Filter
parseNot = do
  _ <- token . string $ "not"
  return (Not)

parseCompare :: Parser Filter
parseCompare = parseEqual <|> parseNotEqual <|> parseGreater <|>
               parseGreaterEqual <|> parseLess <|> parseLessEqual

parseEqual :: Parser Filter
parseEqual = do
  f1 <- parseFilter
  _ <- token . string $ "=="
  f2 <- parseFilter
  return (Equal f1 f2)

parseNotEqual :: Parser Filter
parseNotEqual = do
  f1 <- parseFilter
  _ <- token . string $ "!="
  f2 <- parseFilter
  return (NotEqual f1 f2)

parseGreaterEqual :: Parser Filter
parseGreaterEqual = do
  f1 <- parseFilter
  _ <- token . string $ ">="
  f2 <- parseFilter
  return (GreaterEqual f1 f2)

parseGreater :: Parser Filter
parseGreater = do
  f1 <- parseFilter
  _ <- token . char $ '>'
  f2 <- parseFilter
  return (Greater f1 f2)

parseLessEqual :: Parser Filter
parseLessEqual = do
  f1 <- parseFilter
  _ <- token . string $ "<="
  f2 <- parseFilter
  return (LessEqual f1 f2)

parseLess :: Parser Filter
parseLess = do
  f1 <- parseFilter
  _ <- token . char $ '<'
  f2 <- parseFilter
  return (Less f1 f2)

parseConditional :: Parser Filter
parseConditional = do
  _ <- token . string $ "if"
  f1 <- parseConstructor
  _ <- token . string $ "then"
  f2 <- parseConstructor
  _ <- token . string $ "else"
  f3 <- parseConstructor
  _ <- token . string $ "end"
  return (Conditional f1 f2 f3)

parseTryCatch :: Parser Filter
parseTryCatch = do
  _ <- token . string $ "try"
  f1 <- parseConstructor
  _ <- token . string $ "catch"
  f2 <- parseConstructor
  return (TryCatch f1 f2)

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseConstructor h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
