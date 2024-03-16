module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser

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
  p <- parseOptObjId <|> parseOptObjIdentId <|> parseObjId <|> parseObjIdentId
  ps <- many (parseOptObjId <|> parseOptObjIdentId <|> parseObjId <|> parseObjIdentId)
  return (makePipe (p : ps))

parseObjId :: Parser Filter
parseObjIdentId :: Parser Filter
parseObjIdentId = do
  _ <- token . char $ '.'
  s <- parseKeyQuotes <|> parseKeyNoQuotes
  return (ObjIndex s)
parseObjId = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  _ <- token . char $ '"'
  s <- some (sat (/= '"'))
  _ <- token . char $ '"'
  _ <- token . char $ ']'
  return (ObjIndex s)

--parseOptObjIndex :: Parser Filter
--parseOptObjIndex = parseOptObjId <|> parseOptObjIdentId

parseOptObjId :: Parser Filter
parseOptObjIdentId :: Parser Filter
parseOptObjIdentId = do
  _ <- token . char $ '.'
  s <- parseKeyQuotes <|> parseKeyNoQuotes
  _ <- token . char $ '?'
  return (OptObjIndex s)
parseOptObjId = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  _ <- token . char $ '"'
  s <- some (sat (/= '"'))
  _ <- token . char $ '"'
  _ <- token . char $ ']'
  _ <- token . char $ '?'
  return (OptObjIndex s)

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
  from <- int
  _ <- token . char $ ':'
  to <- int
  _ <- token . char $ ']'
  return (Slice from to)

parseIterator :: Parser Filter
parseIterator = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  _ <- token . char $ ']'
  return Iterator

parseOptIterator :: Parser Filter
parseOptIterator = do
  _ <- token . char $ '.'
  _ <- token . char $ '['
  _ <- token . char $ ']'
  _ <- token . char $ '?'
  return OptIterator

parseComma :: Parser Filter
parseComma = do
  f <- parsePipe
  fs <- many (do _ <- token (char ',')
                 parsePipe)
  return $ makeComma (f : fs)

makeComma :: [Filter] -> Filter
makeComma [] = Identity
makeComma [f] = f
makeComma (f : fs) = Comma f (makeComma fs)

parsePipe :: Parser Filter
parsePipe = do
  f <- parseFilter
  fs <- many (do _ <- token (char '|')
                 parseFilter)
  return $ makePipe (f : fs)

makePipe :: [Filter] -> Filter
makePipe [] = Identity
makePipe [f] = f
makePipe (f : fs) = Pipe f (makePipe fs)

parseFilter :: Parser Filter
parseFilter =  parseParenthesis <|> parseObjIndex <|> parseArrIndex <|>
               parseSlice <|> parseOptIterator <|> parseIterator <|>
               parseIdentity

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
  n <- parseJNumber
  return (Value n)

parseFArray :: Parser Filter
parseFArray = do
  _ <- token . char $ '['
  isEmpty <- token (char ']') <|> return ' '
  if isEmpty == ']'
    then return (Array DoNothing)
    else do
      f <- parseConstructor
      fs <- many (do _ <- token (char ',')
                     parseConstructor)
      _ <- token . char $ ']'
      return (Array (makeComma (f : fs)))

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
  return (ObjectKey s, OptObjIndex s)

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

parseConstructor :: Parser Filter
parseConstructor = parseFNull <|> parseFBool <|> parseFString
                   <|> parseFNumber <|> parseFArray <|> parseFObject <|> parseComma

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseConstructor h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
