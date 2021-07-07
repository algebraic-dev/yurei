module Error 

import Loc
import Syntax.Tokens 

public export
data ExpectType 
  = ExpectTkn Tkn
  | ExpectKeyword String
  | ExpectId
  | ExpectInt
  | ExpectStr
  | Unknown

public export
data BetweenType
  = Parenthesis
  | CurlyBrackets
  | SquareBrackets

public export
data ReaderError
  = NotClosed BetweenType
  | Expected ExpectType Tkn

public export
data ErrorType
  = LexicalError Range
  | ReadingError Range ReaderError
  | EOF
  | ExpectedEOF
  | UnexpectedInternalError

public export 
Show BetweenType where 
  show Parenthesis = "Parenthesis"
  show CurlyBrackets = "CurlyBrackets"
  show SquareBrackets = "SquareBrackets"

Show ExpectType where 
  show (ExpectTkn n) = "ExpectTkn " ++ (show n)
  show (ExpectKeyword a) = "ExpectKeyword " ++ a
  show ExpectId = "ExpectId"
  show ExpectInt = "ExpectInt"
  show ExpectStr = "ExpectStr"
  show Unknown = "Unknown"
  
public export 
Show ReaderError where 
  show (NotClosed n) = "Not Closed " ++ show n
  show (Expected a b) =  "Expected " ++ (show a) ++ " but got " ++ (show b)

public export 
Show ErrorType where
  show (LexicalError a) = "LexicalError "
  show (ReadingError range b) = "ReadingError in line " ++ (cast (Loc.line $ Range.start range)) ++ "  : " ++ (show b)
  show EOF = "EOF"
  show ExpectedEOF = "Expected EOF"
  show UnexpectedInternalError = "Unexpected Internal Error"