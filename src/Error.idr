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