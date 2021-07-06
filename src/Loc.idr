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
  show (MkLoc col line) = "col: " ++ (show col) ++ ", line: " ++ (show line)