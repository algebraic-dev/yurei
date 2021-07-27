module Typing.Term 

import public Syntax.Term 
import public Loc

mutual 

  public export 
  record TyProgram where 
    constructor MkProgram 
    fileName  : String 
    typedDefs : List TyDef
    dataDefs  : List DataDef    
    recordDef : List RecordDef 

  public export
  record TyDef where 
    constructor TyDefine 
    name : Name 
    type : Ty
    value : TyExpr

  record TyPath where 
    constructor MkTypedPath
    path : Path 
    type : Ty

  public export
  data TyExpr 
    = TELambda   Range Ty Name TyExpr
    | TECall     Range Ty TyExpr TyExpr
    | TEDo       Range Ty (List TyExpr)
    | TECase     Range Ty TyExpr (List (TyPat, TyExpr))
    | TEId       Path  Ty 
    | TELit      Range Ty Literal
    
  public export
  data TyPat 
    = TPaId      Name  Ty
    | TPaList    Range Ty (List TyPat)
    | TPaHdTl    Range Ty TyPat TyPat
    | TPaData    Range Ty TyPath (List TyPat)
    | TPaLit     Range Ty Literal

  public export
  data Ty
    = TyPoly     Range Path Ty
    | TySimple   Range String
    | TyArrow    Range Ty Ty
    | TyVar      Range String 

  public export
  data Schema = MkSchema (List String) Ty

  Show TyPath where 
    show (MkTypedPath p _) = show p

  public export 
  Show Ty where 
    show (TyPoly r path ty) = (show path) ++ " " ++ show ty 
    show (TySimple r name) = name
    show (TyVar r name) = name
    show (TyArrow r a b) = show a ++ " -> " ++ show b 