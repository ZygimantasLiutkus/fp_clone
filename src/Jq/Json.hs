module Jq.Json where

import Data.Char (ord)
import Numeric (showHex)

data JSON =
    JString String | JNumber Int | JFloat Float |
    JBool Bool | JNull | JObject [(String, JSON)] | JArray [JSON]

instance Show JSON where
    show json = prettyPrint json 0
        where
            prettyPrint (JNull) _ = "null"
            prettyPrint (JString s) _ = "\"" ++ escapeString s ++ "\""
            prettyPrint (JNumber n) _ = show n
            prettyPrint (JFloat f) _ = show f
            prettyPrint (JBool True) _ = "true"
            prettyPrint (JBool False) _ = "false"
            prettyPrint (JObject o) indent = "{" ++ showJSONObject o ++ "}"
                where
                    showJSONObject [] = ""
                    showJSONObject ((k, v):[]) = "\n" ++ replicate (indent + 2) ' ' ++ show (JString k) ++ ": " ++ prettyPrint v (indent + 2) ++ "\n" ++ replicate indent ' '
                    showJSONObject ((k, v):t) = "\n" ++ replicate (indent + 2) ' ' ++ show (JString k) ++ ": " ++ prettyPrint v (indent + 2) ++ "," ++ showJSONObject t
            prettyPrint (JArray xs) indent = "[" ++ showJSONArray xs ++ "]"
                where
                    showJSONArray [] = ""
                    showJSONArray (h:[]) = "\n" ++ replicate (indent + 2) ' ' ++ prettyPrint h (indent + 2) ++ "\n" ++ replicate indent ' '
                    showJSONArray (h:t) = "\n" ++ replicate (indent + 2) ' ' ++ prettyPrint h (indent + 2) ++ "," ++ showJSONArray t
            escapeString :: String -> String
            escapeString [] = []
            escapeString (c:cs) = case lookup c simpleEscapes of
                Just r -> r ++ escapeString cs
                Nothing
                  | c < ' ' -> hexEscape c ++ escapeString cs -- || c == '\x7f' || c > '\xff'
                  | otherwise -> c : escapeString cs
            simpleEscapes :: [(Char, String)]
            simpleEscapes = zipWith (\a b -> (a, ['\\', b])) "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
            hexEscape :: Char -> String
            hexEscape c = "\\u" ++ replicate (4 - length h) '0' ++ h
                where
                    h = showHex (ord c) ""

instance Eq JSON where
  JNull == JNull = True
  JString s1 == JString s2 = s1 == s2
  JNumber n1 == JNumber n2 = n1 == n2
  JFloat f1 == JFloat f2 = f1 == f2
  JBool b1 == JBool b2 = b1 == b2
  JObject [] == JObject [] = True
  JObject ((s1, v1):t1) == JObject ((s2, v2):t2) = s1 == s2 && v1 == v2 && JObject t1 == JObject t2
  JArray [] == JArray [] = True
  JArray (h1:t1) == JArray (h2:t2) = h1 == h2 && JArray t1 == JArray t2
  _ == _ = False

instance Ord JSON where
  compare (JString s1) (JString s2) = compare s1 s2
  compare (JNumber n1) (JNumber n2) = compare n1 n2
  compare (JFloat f1) (JFloat f2) = compare f1 f2
  compare (JBool b1) (JBool b2) = compare b1 b2
  compare (JObject o1) (JObject o2) = compare o1 o2
  compare (JArray a1) (JArray a2) = compare a1 a2
  compare JNull JNull = EQ
  compare JNull _ = LT
  compare _ JNull = GT
  compare (JBool _) _ = LT
  compare _ (JBool _) = GT
  compare (JNumber n) (JFloat f) = compare (fromIntegral n) f
  compare (JFloat f) (JNumber n) = compare f (fromIntegral n)
  compare (JNumber _) _ = LT
  compare _ (JNumber _) = GT
  compare (JFloat _) _ = LT
  compare _ (JFloat _) = GT
  compare (JString _) _ = LT
  compare _ (JString _) = GT
  compare (JArray _) _ = LT
  compare _ (JArray _) = GT
  compare (JObject _) _ = GT
  compare _ (JObject _) = LT

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to JSON datatype
-- For the tests to succeed fill them in with functions that return correct JSON values
-- Don't change the names or signatures, only the definitions

jsonNullSC :: JSON
jsonNullSC = JNull

jsonNumberSC :: Int -> JSON
jsonNumberSC x = JNumber x

jsonStringSC :: String -> JSON
jsonStringSC s = JString s

jsonBoolSC :: Bool -> JSON
jsonBoolSC b = JBool b

jsonArraySC :: [JSON] -> JSON
jsonArraySC xs = JArray xs

jsonObjectSC :: [(String, JSON)] -> JSON
jsonObjectSC o = JObject o
