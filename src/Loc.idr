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
  show (MkLoc {column, line}) = "col: " ++ (show column) ++ ", line: " ++ (show line)

public export
mixRange : Range -> Range -> Range
mixRange (MkRange start _) (MkRange _ end) = (MkRange start end)

