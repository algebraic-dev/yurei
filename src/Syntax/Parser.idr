module Syntax.Parser 

import Modified.Parser
import Modified.Core
import Syntax.Lexer
import Syntax.Tokens
import Error
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
Parser ty = Grammar GrammarError (Range, Tkn) True ty

-- These are the "primitive" functions to make the parsing 

token : Tkn -> Parser Range
token expected = terminal 
  (ErrorCustom . (\(loc, tkn) => Expected loc expected tkn)) 
  (\(loc, actual) => if actual == expected then Just loc else Nothing)

keyword : String -> Parser Range
keyword expected = terminal 
  (ErrorCustom . (\(loc, tkn) => ExpectedKeyword loc expected tkn)) 
  (\(loc, actual) => case actual of 
                      TknId x => if x == expected then Just loc else Nothing
                      _       => Nothing) 

identifier : Parser (Range, String)
identifier = terminal 
  (ErrorCustom . (\(loc, tkn) => ExpectedEmpty loc (TknId "") tkn)) 
  (\(loc, actual) => case actual of 
                      TknId x => Just (loc, x)
                      _       => Nothing) 

integer : Parser (Range, Int)
integer = terminal 
  (ErrorCustom . (\(loc, tkn) => ExpectedEmpty loc (TknNum 0) tkn)) 
  (\(loc, actual) => case actual of 
                      TknNum x => Just (loc, x)
                      _       => Nothing) 

string : Parser (Range, String)
string = terminal 
  (ErrorCustom . (\(loc, tkn) => ExpectedEmpty loc (TknStr "") tkn)) 
  (\(loc, actual) => case actual of 
                      TknStr x => Just (loc, x)
                      _       => Nothing) 

