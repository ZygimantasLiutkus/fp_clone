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
compile (ArrIndex i) inp = case inp of
  JArray a -> if i < length a && i >= 0
                then return [a !! i]
                else if i < 0 && abs i <= length a
                  then return [a !! (length a + i)]
                  else return [JNull]
  JNull -> return [JNull]
  _ -> Left "Array index not applicable"
compile (Slice from to) inp = do
  from' <- compile from inp
  to' <- compile to inp
  (fs, ts) <- mapJSONSlice (from', to')
  case inp of
    JArray a -> let ps = map (getSlice (length a)) [(x, y) | x <- fs, y <- ts]
                in return $ map (\(f, t) -> JArray $ take (t - f) $ drop f a) ps
    JString s -> let ps = map (getSlice (length s)) [(x, y) | x <- fs, y <- ts]
                 in case ps of
                   [(f, t)] -> return [JString $ take (t - f) $ drop f s]
                   _ -> Left "Cannot iterate over string"
    JNull -> return [JNull]
    _ -> Left "Slice not applicable"
    where
      getSlice :: Int -> (Int, Int) -> (Int, Int)
      getSlice l p = case p of
        (f, t) | f >= 0 && t >= 0 -> (f, t)
        (f, t) | f < 0 && t >= 0 -> let f' = if abs f > l then 0 else l + f
                                   in (f', t)
        (f, t) | f >= 0 && t < 0 -> let t' = if abs t > l then 0 else l + t
                                   in (f, t')
        (f, t) | f < 0 && t < 0 -> let f' = if abs f > l then 0 else l + f
                                       t' = if abs t > l then 0 else l + t
                                   in (f', t')

compile (Iterator arr) inp = case inp of
  JArray a -> if null arr then return a else do
    ids <- mapM (\x -> compile x inp) arr
    r <- mapJSONIdent (concat ids)
    res <- mapM (\x -> compile x inp) r
    return $ concat res
  JObject o -> if null arr then return $ map snd o else do
    ids <- mapM (\x -> compile x inp) arr
    r <- mapJSONIdent (concat ids)
    res <- mapM (\x -> compile x inp) r
    return $ concat res
  JNull -> if null arr then Left "Cannot iterate over null" else do
    ids <- mapM (\x -> compile x inp) arr
    r <- mapJSONIdent (concat ids)
    res <- mapM (\x -> compile x inp) r
    return $ concat res
  _ -> Left "Iterator not applicable"
compile (Optional f) inp = case compile f inp of
  Right x -> return x
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
compile (ObjectKey s) _ = return [JString s]
compile (DoNothing) _ = return []
compile (Descent) inp = do
  r <- remainDescent inp
  return (inp:r)
compile (And f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (mapBool a) && (mapBool b)]
compile (Or f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (mapBool a) || (mapBool b)]
compile (Not) inp = return [JBool (not (mapBool inp))]
compile (Equal f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (a == b)]
compile (NotEqual f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (a /= b)]
compile (Greater f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (a > b)]
compile (GreaterEqual f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (a >= b)]
compile (Less f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (a < b)]
compile (LessEqual f1 f2) inp = do
  r1 <- compile f1 inp
  r2 <- compile f2 inp
  return [JBool x | a <- r1, b <- r2, let x = (a <= b)]
compile (Conditional c t f) inp = do
  r1 <- compile c inp
  r2 <- compile t inp
  r3 <- compile f inp
  return $ concat [if mapBool a then r2 else r3 | a <- r1]

remainDescent :: JProgram [JSON]
remainDescent inp = do
  r <- compile (Optional (Iterator [])) inp
  if null r
    then return []
    else do
      rs <- mapM remainDescent r
      return $ r ++ concat rs

mapBool :: JSON -> Bool
mapBool (JBool b) = b
mapBool (JNull) = False
mapBool _ = True

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j


mapJSONIdent :: [JSON] -> Either String [Filter]
mapJSONIdent [] = return []
mapJSONIdent ((JNumber n):xs) = do
  ns <- mapJSONIdent xs
  return ((ArrIndex n):ns)
mapJSONIdent ((JString s):xs) = do
  ss <- mapJSONIdent xs
  return ((ObjIndex s):ss)
mapJSONIdent (_:xs) = do
  xss <- mapJSONIdent xs
  return (DoNothing:xss)

mapJSONSlice :: ([JSON], [JSON]) -> Either String ([Int], [Int])
mapJSONSlice (from, to) = do
  fs <- mapInts False from
  ts <- mapInts True to
  return (fs, ts)

mapInts :: Bool -> [JSON] -> Either String [Int]
mapInts _ [] = return []
mapInts b ((JNumber n):xs) = do
  ns <- mapInts b xs
  return (n:ns)
mapInts b (JNull:xs) = do
  n <- if b then return (maxBound :: Int) else return 0
  ns <- mapInts b xs
  return (n:ns)
mapInts _ _ = Left "Cannot slice with non-integer"