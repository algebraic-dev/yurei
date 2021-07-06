module Syntax.Parser 

import Modified.Parser
import Modified.Core
import Syntax.Lexer
import Loc

-- the type variable "a" means annotation. 
-- We can put multiple types of data here like types and ranges

mutual 
  data Path a 
    = PDot     a (a, String) (Path a)
    | PName    a String 

  data TopLevel a 
    = TLDecl   a (a, String) (List (Types a)) (List (Pat a)) (Expr a)

  data Expr a
    = ELit     a (Literal a)
    | ELambda  a (Pat a) (Expr a)
    | ECall    a (Expr a) (Expr a)
    | EDo      a (List (Expr a))
    | EId      a (Path a)

  data Pat a 
    = PId      a String

  data Literal a 
    = LStr     a String 
    | LNum     a Int 

  data Types a
    = TSimple  a String
    | TArrow   a (Types a) (Types a)
    | TPoly    a (a, Path a) (Types a)

-- Parser type is the entry point of the parser, It receives Locs and Tkns and
-- Will return in the end a (List (TopLevel Range)).

Parser : Type -> Type
Parser ty = Grammar Int (Loc, Tkn) True ty

termLoc : Tkn -> Parser Loc
termLoc expected = terminal (ErrorCustom 3)
     (\(loc, actual) => if actual == expected then Just loc else Nothing)

term : Tkn -> Parser ()
term expected = terminal (ErrorCustom 2)
     (\(_, actual) => if actual == expected then Just () else Nothing)

teste : Parser ()
teste = fail (ErrorCustom 2)
