module Term 


mutual 
  data Path a 
    = PDot      a (a, String) (Path a)
    | PName     a String 

  data TopLevel a 
    = TLDecl    a (a, String) (List (Types a)) (List (Pat a)) (Expr a)

  data Expr a
    = ELit      a (Literal a)
    | ELambda   a (Pat a) (Expr a)
    | ECall     a (Expr a) (Expr a)
    | EDo       a (List (Expr a))
    | EId       a (Path a)

  data Pat a 
    = PId       a String

  data Literal  a 
    = LStr      a String 
    | LNum      a Int 

  data Types a
    = TSimple   a String
    | TArrow    a (Types a) (Types a)
    | TPoly     a (a, Path a) (Types a)
    