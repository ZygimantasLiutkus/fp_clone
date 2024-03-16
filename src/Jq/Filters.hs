module Jq.Filters where

import Jq.Json

data Filter = Identity | Parenthesis Filter | ObjIndex String | OptObjIndex String |
              ArrIndex Int | Slice Int Int | Iterator | OptIterator | Comma Filter Filter |
              Pipe Filter Filter | Value JSON | Array Filter | Object [(Filter, Filter)] |
              ObjectKey String | DoNothing


instance Show Filter where
  show (Identity) = "."
  show (Parenthesis f) = "(" ++ show f ++ ")"
  show (ObjIndex s) = "." ++ s
  show (OptObjIndex s) = "." ++ s ++ "?"
  show (ArrIndex i) = ".["++show i++"]"
  show (Slice from to) = ".["++ show from ++":"++show to++"]"
  show (Iterator) = ".[]"
  show (OptIterator) = ".[]?"
  show (Comma f1 f2) = show f1 ++ " , " ++ show f2
  show (Pipe f1 f2) = show f1 ++ " | " ++ show f2

instance Eq Filter where
  Identity == Identity = True
  Parenthesis f1 == Parenthesis f2 = f1 == f2
  ObjIndex s1 == ObjIndex s2 = s1 == s2
  OptObjIndex s1 == OptObjIndex s2 = s1 == s2
  ArrIndex i == ArrIndex j = i == j
  Slice i j == Slice x y = i == x && j == y
  Iterator == Iterator = True
  OptIterator == OptIterator = True
  Comma f1 f2 == Comma f3 f4 = f1 == f3 && f2 == f4
  Pipe f1 f2 == Pipe f3 f4 = f1 == f3 && f2 == f4
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
