module Syntax.Term 
import public Loc
import Data.String 
import Data.List1

formatText : String -> Nat -> String
formatText text ident =
  let identation = replicate ident ' ' in
  unlines $ map ((++) identation) $ (forget $ lines text)

mutual 
  public export 
  record Program where 
    constructor MkProgram 
    fileName  : String 
    dataDefs  : List DataDef 
    defs      : List Def   
    recordDef : List RecordDef 

  public export 
  record Name where 
    constructor MkName
    range: Range 
    name: String

  public export 
  record DataDef where
    constructor DefData 
    name : Name 
    type : Maybe Types 
    fields : List (Name, Maybe Types)

  public export
  record Def where 
    constructor Define 
    name : Name 
    type : Maybe Types
    value : Expr
    
  public export 
  record RecordDef where
    constructor DefRecord 
    name : Name 
    type : Maybe Types 
    fields : List (Name, Types)

  public export
  data Path 
    = PDot      Range Name Path
    | PName     Range String 

  public export
  data Expr 
    = ELit      Range Literal
    | ELambda   Range Name Expr
    | ECall     Range Expr Expr
    | EDo       Range (List Expr)
    | ECase     Range (List (Expr, Expr))
    | EId       Path

  public export
  data Pat 
    = PaId       Name
    | PaLit      Range Literal
    | PaData     Range Path (List Pat)
    | PaList     Range (List Pat)

  public export
  data Literal 
    = LStr      Range String 
    | LInt      Range Int 

  public export
  data Types
    = TSimple   Range String
    | TArrow    Range Types Types
    | TPoly     Range Path Types
    | TVar      Range String 
  
  Show Name where 
    show (MkName range n) = n
  
  Show Path where 
    show (PDot r name path) = (show name) ++ "." ++ (show path)
    show (PName r str) = str 

  Show Literal where 
    show (LStr r str) = "\"" ++ str ++ "\""
    show (LInt r int) = show int 
  
  Show Expr where 
    show (ELit r lit) = show lit 
    show (ELambda r arg expr) = "(λ [" ++ (show arg) ++ "] " ++ (show expr) ++ ")"
    show (EDo r exprs) = "(Do\n" ++ formatText (unlines $ map show exprs) 2 ++ ")"
    show (ECase r exprs) = "(case\n  " ++ formatText (unlines $ map show exprs) 2 ++ ")"
    show (ECall r a b) = concat ["(", show a, " ", show b,")"]
    show (EId n) = show n

  Show Pat where 
    show (PaId n) = show n 
    show (PaLit r l) = show l 
    show (PaData r path pats) = "(" ++ show path ++ ", " ++ (concatMap ((\a => a ++ ",") . show) pats)  ++ ")"
    show (PaList r list) = "[" ++ (concatMap ((\a => a ++ ", ") . show) list) ++ "]"

  Show Types where 
    show (TSimple r s) = s 
    show (TArrow r a b) = show a ++ " -> " ++ show b 
    show (TPoly r path n) = "(" ++ (show path) ++ " " ++ show n ++ ")"
    show (TVar a s) = s

  Show DataDef where 
    show (DefData name type fields) = 
      "Data: \n  " ++ (formatText (concat [
        "Name: \n    ", show name, "\n",
        "Type: \n    ", show type, "\n",
        "Fields: \n", (formatText (
          unlines $ map (\(name, type) => concat ["(", show name,":",show type, ")"]) fields
        ) 2)
      ]
      ) 2)

  Show RecordDef where 
    show (DefRecord name type fields) = 
      "Record: \n" ++ (formatText (concat [
        "Name:\n    ", show name, "\n",
        "Type: \n    ", show type, "\n",
        "Fields: \n", (formatText (
          unlines $ map (\(name, type) => concat ["(", show name," : ",show type, ")"]) fields
        ) 2)
      ]
      ) 2)

  Show Def where 
    show (Define name type expr) = 
      "Def: \n" ++ (formatText (concat [
        "Name: \n    ", show name, "\n",
        "Type: \n    ", show type, "\n",
        "Value: \n", (formatText (show expr) 2)]
      ) 2)

  public export
  Show Program where 
    show (MkProgram name datad defs recs) = 
      "Program: \n" ++ (formatText (concat [
        "Datas: \n", (formatText (show datad) 2), "\n",
        "Defs: \n", (formatText (show defs) 2), "\n",
        "Records: \n", (formatText (show recs) 2)]
      ) 2)

  

  