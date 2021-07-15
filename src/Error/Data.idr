module Error.Data

import public Loc
import public Syntax.Tokens 

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
data CapitalizedNameErr
  = CapitalModule
  | CapitalType
  | CapitalADTName 
  | CapitalDataConstructor

public export 
data ParserError 
  = InvalidTopLevel String
  | InvalidIdName String
  | InvalidPath String
  | ExpectedDataField
  | ExpectedLiteral 
  | ExpectedIdentifier
  | ExpectedIdButGotPath
  | ExpectedTypeDef
  | ImpossibleParsingError
  | NeedCapitalizedName CapitalizedNameErr
  | NeedMinusculeName
  | NotSupportEmptyList
  | NotAValidPattern
  | NotAValidType 
  | NotAValidRecordField
  | InvalidExpr
  | Unreachable
  | NotAllCasesHaveCond

public export
data ErrorType
  = LexicalError Range LexerError
  | ReadingError Range ReaderError
  | ParsingError Range ParserError
  | EOF
  | ExpectedEOF
  | UnexpectedInternalError

public export
record ErrorData where
  constructor MkErrorData
  fileName    : String 
  sourceInput : String 
  error       : ErrorType 
