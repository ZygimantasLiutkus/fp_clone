module Jq.Filters where

import Jq.Json

data Filter = Identity | Parenthesis Filter | ObjIndex String | ArrIndex Int |
              Slice Filter Filter | Iterator [Filter] | Optional Filter | Comma Filter Filter |
              Pipe Filter Filter | Value JSON | Array Filter |
              Object [(Filter, Filter)] | ObjectKey String | DoNothing |
              Descent | And Filter Filter | Or Filter Filter | Not |
              Equal Filter Filter | NotEqual Filter Filter | Greater Filter Filter |
              GreaterEqual Filter Filter | Less Filter Filter | LessEqual Filter Filter |
              Conditional Filter Filter Filter | TryCatch Filter Filter


instance Show Filter where
  show (Identity) = "."
  show (Parenthesis f) = "(" ++ show f ++ ")"
  show (ObjIndex s) = "." ++ s
  show (ArrIndex i) = ".["++show i++"]"
  show (Slice from to) = ".["++ show from ++":"++show to++"]"
  show (Iterator arr) = "." ++ show arr
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
  show (And f1 f2) = show f1 ++ " and " ++ show f2
  show (Or f1 f2) = show f1 ++ " or " ++ show f2
  show (Not) = "not"
  show (Equal f1 f2) = show f1 ++ " == " ++ show f2
  show (NotEqual f1 f2) = show f1 ++ " != " ++ show f2
  show (Greater f1 f2) = show f1 ++ " > " ++ show f2
  show (GreaterEqual f1 f2) = show f1 ++ " >= " ++ show f2
  show (Less f1 f2) = show f1 ++ " < " ++ show f2
  show (LessEqual f1 f2) = show f1 ++ " <= " ++ show f2
  show (Conditional f1 f2 f3) = "if " ++ show f1 ++ " then " ++ show f2 ++ " else " ++ show f3 ++ " end"
  show (TryCatch f1 f2) = "try " ++ show f1 ++ " catch " ++ show f2

instance Eq Filter where
  Identity == Identity = True
  Parenthesis f1 == Parenthesis f2 = f1 == f2
  ObjIndex s1 == ObjIndex s2 = s1 == s2
  ArrIndex i == ArrIndex j = i == j
  Slice i j == Slice x y = i == x && j == y
  Iterator a1 == Iterator a2 = a1 == a2
  Optional f1 == Optional f2 = f1 == f2
  Comma f1 f2 == Comma f3 f4 = f1 == f3 && f2 == f4
  Pipe f1 f2 == Pipe f3 f4 = f1 == f3 && f2 == f4
  Value v1 == Value v2 = v1 == v2
  Array f1 == Array f2 = f1 == f2
  Object fs1 == Object fs2 = fs1 == fs2
  ObjectKey s1 == ObjectKey s2 = s1 == s2
  DoNothing == DoNothing = True
  Descent == Descent = True
  And f1 f2 == And f3 f4 = f1 == f3 && f2 == f4
  Or f1 f2 == Or f3 f4 = f1 == f3 && f2 == f4
  Not == Not = True
  Equal f1 f2 == Equal f3 f4 = f1 == f3 && f2 == f4
  NotEqual f1 f2 == NotEqual f3 f4 = f1 == f3 && f2 == f4
  Greater f1 f2 == Greater f3 f4 = f1 == f3 && f2 == f4
  GreaterEqual f1 f2 == GreaterEqual f3 f4 = f1 == f3 && f2 == f4
  Less f1 f2 == Less f3 f4 = f1 == f3 && f2 == f4
  LessEqual f1 f2 == LessEqual f3 f4 = f1 == f3 && f2 == f4
  Conditional f1 f2 f3 == Conditional f4 f5 f6 = f1 == f4 && f2 == f5 && f3 == f6
  TryCatch f1 f2 == TryCatch f3 f4 = f1 == f3 && f2 == f4
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
