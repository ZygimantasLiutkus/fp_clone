module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]
compile (Parenthesis f) inp = compile f inp
compile (ObjIndex s) inp = case inp of
  JObject o -> case lookup s o of
    Just v -> return [v]
    Nothing -> return [JNull]
  JNull -> return [JNull]
  _ -> Left "Object index not applicable"
compile (OptObjIndex s) inp = case inp of
  JObject o -> case lookup s o of
    Just v -> return [v]
    Nothing -> return [JNull]
  JNull -> return [JNull]
  _ -> return []
compile (ArrIndex i) inp = case inp of
  JArray a -> if i < length a && i >= 0
                then return [a !! i]
                else if i < 0 && abs i <= length a
                  then return [a !! (length a + i)]
                  else return [JNull]
  JNull -> return [JNull]
  _ -> Left "Array index not applicable"
compile (Slice from to) inp = case inp of
  JArray a -> case (from, to) of
    (f, t) | f > 0 && t > 0 -> return $ take (t - f) $ drop f a
    (f, t) | f < 0 && t > 0 -> let f' = if abs f > length a then 0 else length a + f
                               in return $ take (t - f') $ drop f' a
    (f, t) | f > 0 && t < 0 -> let t' = if abs t > length a then 0 else length a + t
                               in return $ take (t' - f) $ drop f a
    (f, t) | f < 0 && t < 0 -> let f' = if abs f > length a then 0 else length a + f
                                   t' = if abs t > length a then 0 else length a + t
                               in return $ take (t' - f') $ drop f' a
    _ -> return []
  JNull -> return [JNull]
  _ -> Left "Slice not applicable"
compile (Iterator) inp = case inp of
  JArray a -> return a
  JObject o -> return $ map snd o
  _ -> Left "Iterator not applicable"
compile (OptIterator) inp = case inp of
  JArray a -> return a
  JObject o -> return $ map snd o
  _ -> return []
compile (Comma f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return $ r1 ++ r2
compile (Pipe f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- mapM (compile f2) r1
  return $ concat r2
compile (Value v) _ = return [v]
compile (Array f) inp = do
  r <- compile f inp
  return [JArray r]
compile (Object fs) inp = do
  r <- mapM (\(k, v) ->
    case (compile k inp, compile v inp) of
      (Right [s], Right [val]) -> case s of
        JString key -> return (key, val)
        _ -> Left "Value construction: key is not a string"
      _ -> Left "Value construction: bad object") fs
  return [JObject r]
compile (ObjectKey s) inp = return [JString s]
compile (DoNothing) _ = return []

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
