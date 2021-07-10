module Error.Data

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
  | TknEOF

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
data LexerError 
  = UnterminatedString
  | UnexpectedChar

public export
data ErrorType
  = LexicalError Range LexerError
  | ReadingError Range ReaderError
  | EOF
  | ExpectedEOF
  | UnexpectedInternalError

public export
record ErrorData where
  constructor MkErrorData
  fileName    : String 
  sourceInput : String 
  error       : ErrorType 
