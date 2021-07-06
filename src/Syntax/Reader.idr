module Syntax.Reader 

import Modified.Parser
import Syntax.Tokens
import Error
import Loc

public export
data TermExpr 
  = TList Range TermExpr 
  | TStr  Range String 
  | TInt  Range Int 
  | TId   Range String
  
-- Rule type is the entry point of the reader, It receives Locs and Tkns and
-- Will return in the end a (List (TopLevel Range)).

Rule : Type -> Type
Rule ty = Grammar ErrorType (Range, Tkn) True ty

-- These are the "primitive" functions to make the parsing 
expect : Range -> ExpectType -> Tkn -> ParserError ErrorType
expect loc exp actual = 
  ErrorCustom $ ReadingError loc $ Expected exp actual

error : Range -> ReaderError -> ParserError ErrorType
error loc err = 
  ErrorCustom $ ReadingError loc err

tknRange : Tkn -> Rule Range
tknRange expected = terminal (\(loc, actual) => 
    if actual == expected
      then Right $ loc 
      else Left  $ expect loc (ExpectTkn expected) actual)

tkn : Tkn -> Rule ()
tkn res = do 
  resP <- tknRange res
  pure ()  

identifier : Rule TermExpr
identifier = terminal (\(loc, actual) => 
  case actual of 
    TknId x => Right $ TId loc x
    _       => Left  $ expect loc ExpectId actual)

integer : Rule TermExpr
integer = terminal (\(loc, actual) => 
  case actual of 
    TknNum x => Right $ TInt loc x
    _        => Left  $ expect loc ExpectInt actual)


string : Rule TermExpr
string = terminal (\(loc, actual) => 
  case actual of 
    TknStr x => Right $ TStr loc x
    _        => Left  $ expect loc ExpectStr actual)

-- Actual rules for parsing

failClosedPar : BetweenType -> Range -> Rule ()
failClosedPar between range = fail (error range $ NotClosed between)

between : BetweenType -> Rule Range -> Rule () -> Rule b -> Rule b 
between betweenErr start end middle = do
  range <- start
  res <- middle 
  (end <|> failClosedPar betweenErr range)
  pure res

mutual
  list : Rule TermExpr
  list = 
    between Parenthesis (tknRange TknLPar) (tkn TknRPar) expr 

  set : Rule TermExpr
  set = 
    between CurlyBrackets (tknRange TknLCurly) (tkn TknRCurly) expr 

  square : Rule TermExpr
  square = 
    between SquareBrackets (tknRange TknLSquare) (tkn TknRSquare) expr 

  expr : Rule TermExpr 
  expr = list 
     <|> set 
     <|> square
     <|> string
     <|> identifier
     <|> integer

public export
readToTerm : List (Range,Tkn) -> Either ErrorType TermExpr
readToTerm ls = 
  case parse expr ls of 
    Right (res, []) => Right res 
    Right (res, _) => Left ExpectedEOF
    Left (ExpectedEndOfInput, _) => Left ExpectedEOF
    Left (EndOfInput, _) => Left EOF 
    Left (ErrorCustom err, _) => Left err
    Left _ => Left UnexpectedInternalError
