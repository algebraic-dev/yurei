module Syntax.Reader 

import Modified.Parser
import Syntax.Tokens
import Error
import Data.List1
import Loc
 
public export
data TermExpr 
  = TList Range (List TermExpr) 
  | TStr  Range String 
  | TInt  Range Int 
  | TId   Range String


public export
Show TermExpr where
  show (TList _ list) = "(" ++ (foldl ((++) . (++ " ")) "" (map show list)) ++ ")"
  show (TStr _ str)   = "\"" ++ str ++ "\""
  show (TInt _ n)     = cast n
  show (TId _ n)      = n
  
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

failClosedPar : BetweenType -> Range -> Rule Range
failClosedPar between range = 
  fail (error range $ NotClosed between)


traceRE : IO () -> b -> IO b
traceRE rule return = do 
    rule
    pure return

trace : IO () -> b -> b
trace thing reg = unsafePerformIO (traceRE thing reg)

mutual
  call : Rule TermExpr
  call = do
    range    <- tknRange TknLPar
    res      <- many expr
    endRange <- tknRange TknRPar <|> failClosedPar Parenthesis range
    pure $ TList (mixRange range endRange) res

  list : Rule TermExpr
  list = do
    range    <- tknRange TknLSquare
    res      <- many expr
    endRange <- tknRange TknRSquare <|> failClosedPar SquareBrackets range
    mixedRange <- pure $ mixRange range endRange 
    pure $ TList mixedRange ((TId mixedRange "list") :: res)

  set : Rule TermExpr
  set = do
    range    <- tknRange TknLCurly
    res      <- many expr
    endRange <- tknRange TknRCurly <|> failClosedPar CurlyBrackets range
    mixedRange <- pure $ mixRange range endRange 
    pure $ TList mixedRange ((TId mixedRange "set") :: res)  

  expr : Rule TermExpr 
  expr = string
     <|> identifier
     <|> integer
     <|> list 
     <|> call
     <|> set

  program : Rule (List TermExpr)
  program = do 
    res <- forget <$> some call
    pure res

public export
readToTerm : List (Range,Tkn) -> Either ErrorType (List TermExpr)
readToTerm ls = 
  case parse program ls of 
    Right (res, []) => Right res
    Right (res, (r, hd) :: _) => Left (ReadingError r $ Expected Unknown hd) 
    Left (ExpectedEndOfInput, _) => Left ExpectedEOF
    Left (EndOfInput, _) => Left EOF 
    Left (ErrorCustom err, _) => Left err
    Left _ => Left UnexpectedInternalError
