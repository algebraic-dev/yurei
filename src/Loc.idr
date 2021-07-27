module Loc

public export
record Loc where
  constructor MkLoc
  column, line : Int

public export
record Range where 
  constructor MkRange
  start, end : Loc

public export
Show Loc where 
  show (MkLoc {column, line}) = "(" ++ (show column) ++ ", " ++ (show line) ++ ")"

public export
Show Range where 
  show (MkRange {start, end}) = "(" ++ (show start) ++ ", " ++ (show end) ++ ")"

public export
mixRange : Range -> Range -> Range
mixRange (MkRange start _) (MkRange _ end) = (MkRange start end)

public export
emptyRange : Range 
emptyRange = MkRange (MkLoc 0 0) (MkLoc 0 0)