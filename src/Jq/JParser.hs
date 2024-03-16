module Jq.JParser where

import Parsing.Parsing
import Jq.Json

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJString :: Parser JSON
parseJString = do
  s <- parseString
  return (JString s)

parseString :: Parser String
parseString = do
   _ <- char '"'
   s <- many (escapeChar <|> sat (/= '"'))
   _ <- char '"'
   return s

escapeChar :: Parser Char
escapeChar = do
  _ <- char '\\'
  c <- sat (`elem` ['"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u'])
  case c of
    '"' -> return '"'
    '\\' -> return '\\'
    '/' -> return '/'
    'b' -> return '\b'
    'f' -> return '\f'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'u' -> parseUnicode
    _ -> empty

parseUnicode :: Parser Char
parseUnicode = do
  h1 <- hexDigit
  h2 <- hexDigit
  h3 <- hexDigit
  h4 <- hexDigit
  return (toEnum (read ("0x" ++ [h1, h2, h3, h4]) :: Int) :: Char)

hexDigit :: Parser Char
hexDigit = digit <|> sat (`elem` ['a'..'f']) <|> sat (`elem` ['A'..'F'])

parseJNumber :: Parser JSON
parseJNumber = do
  n <- int
  return (JNumber n)

parseJBool :: Parser JSON
parseJBool = do
  b <- string "true" <|> string "false"
  return (JBool (b == "true"))

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = do
  x <- token p
  xs <- many (do _ <- token sep
                 y <- token p
                 return y)
  return (x:xs)

parseJArray :: Parser JSON
parseJArray = do
  _ <- char '['
  isEmpty <- token (char ']') <|> return ' '
  if isEmpty == ']'
    then return (JArray [])
    else do
      xs <- sepBy parseJSON (char ',')
      _ <- char ']'
      return (JArray xs)

parseJObject :: Parser JSON
parseJObject = do
  _ <- token . char $ '{'
  isEmpty <- token (char '}') <|> return ' '
  if isEmpty == '}'
    then return (JObject [])
    else do
      kvs <- sepBy parseKV (char ',')
      _ <- char '}'
      return (JObject kvs)
      where
        parseKV = do
          k <- parseString
          _ <- token . char $ ':'
          v <- parseJSON
          return (k, v)

parseJSON :: Parser JSON
parseJSON = parseJNull <|> parseJString <|> parseJNumber
            <|> parseJBool <|> parseJArray <|> parseJObject
