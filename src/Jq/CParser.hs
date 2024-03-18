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
  _ <- token . char $ '"'
  s <- some (sat (/= '"'))
  _ <- token . char $ '"'
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
  f <- parseComma
  fs <- many (do _ <- token (char '|')
                 parseComma)
  return $ makePipe (f : fs)

makePipe :: [Filter] -> Filter
makePipe [] = Identity
makePipe [f] = f
makePipe (f : fs) = Pipe f (makePipe fs)

parseFilter :: Parser Filter
parseFilter =  do
  f <- parseParenthesis <|> parseValue <|> parseObjIndex <|>
       parseSlice <|> parseIterator <|> parseDescent <|> parseIdentity
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
parseConstructor = parsePipe

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseConstructor h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
