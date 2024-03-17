module Jq.Filters where

import Jq.Json

data Filter = Identity | Parenthesis Filter | ObjIndex String | ArrIndex Int |
              Slice Int Int | Iterator | Optional Filter | Comma Filter Filter |
              Pipe Filter Filter | Value JSON | Array Filter |
              Object [(Filter, Filter)] | ObjectKey String | DoNothing |
              Descent


instance Show Filter where
  show (Identity) = "."
  show (Parenthesis f) = "(" ++ show f ++ ")"
  show (ObjIndex s) = "." ++ s
  show (ArrIndex i) = ".["++show i++"]"
  show (Slice from to) = ".["++ show from ++":"++show to++"]"
  show (Iterator) = ".[]"
  show (Optional f) = show f ++ "?"
  show (Comma f1 f2) = show f1 ++ " , " ++ show f2
  show (Pipe f1 f2) = show f1 ++ " | " ++ show f2
  show (Value v) = show v
  show (Array f) = "[" ++ show f ++ "]"
  show (Object fs) = "{" ++ showFields fs ++ "}"
    where showFields [] = ""
          showFields [(k, v)] = show k ++ " : " ++ show v
          showFields ((k, v):xs) = show k ++ " : " ++ show v ++ ", " ++ showFields xs
  show (ObjectKey s) = s
  show (DoNothing) = ""
  show (Descent) = ".."

instance Eq Filter where
  Identity == Identity = True
  Parenthesis f1 == Parenthesis f2 = f1 == f2
  ObjIndex s1 == ObjIndex s2 = s1 == s2
  ArrIndex i == ArrIndex j = i == j
  Slice i j == Slice x y = i == x && j == y
  Iterator == Iterator = True
  Optional f1 == Optional f2 = f1 == f2
  Comma f1 f2 == Comma f3 f4 = f1 == f3 && f2 == f4
  Pipe f1 f2 == Pipe f3 f4 = f1 == f3 && f2 == f4
  Value v1 == Value v2 = v1 == v2
  Array f1 == Array f2 = f1 == f2
  Object fs1 == Object fs2 = fs1 == fs2
  ObjectKey s1 == ObjectKey s2 = s1 == s2
  DoNothing == DoNothing = True
  Descent == Descent = True
  _ == _ = False

data Config = ConfigC {filters :: Filter}

-- Smart constructors
-- These are included for test purposes and
-- aren't meant to correspond one to one with actual constructors you add to Filter
-- For the tests to succeed fill them in with functions that return correct filters
-- Don't change the names or signatures, only the definitions

filterIdentitySC :: Filter
filterIdentitySC = Identity

filterStringIndexingSC :: String -> Filter
filterStringIndexingSC s = ObjIndex s

filterPipeSC :: Filter -> Filter -> Filter
filterPipeSC f1 f2 = Pipe f1 f2

filterCommaSC :: Filter -> Filter -> Filter
filterCommaSC f1 f2 = Comma f1 f2
