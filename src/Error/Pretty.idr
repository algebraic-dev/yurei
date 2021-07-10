module Error.Pretty

import Data.String
import Data.List1
import Error.Data
import Error.Format
import Error.ANSI
import Syntax.Tokens 
import Loc

record ErrInfo where 
  constructor MKRangedErrInfo 
  lines : List String 
  mainLine : String 
  filename : String 
  errorLineNum : (Int, Int)
  defaultRange : Range 

-- Helper function 

orElse : Maybe a -> a -> a 
orElse maybe alternative = 
  case maybe of 
    Just x => x 
    Nothing => alternative

-- Patterns of error messages

commonErrorMessage : ErrInfo -> Maybe Range -> String -> String 
commonErrorMessage errInfo maybeRange text = 
  let range = orElse maybeRange (ErrInfo.defaultRange errInfo)
      mainLine = Loc.line $ Range.start range
      errRange = (ErrInfo.errorLineNum errInfo) 
      markLine = createMarkLine Fg.red (cast $ length (ErrInfo.mainLine errInfo)) range 
  in 
  unlines [ formatHeader text
          , fileHeader range (ErrInfo.filename errInfo)
          , unlines $ 
            insertInLines errRange (mainLine + 1) markLine
            (formatLines range errRange mainLine (ErrInfo.lines errInfo))
          , lineJump 1]

-- Lexer error messages 

getLexerError : ErrInfo -> Range -> LexerError -> String 

getLexerError err range UnterminatedString = 
  commonErrorMessage err (Just range) "You forgot to terminate this string with another double quote!"

getLexerError err range UnexpectedChar =
  commonErrorMessage err (Just range) "You cannot use this character here!"

-- Reader error helpers

getBetweenTypeName : BetweenType -> String 
getBetweenTypeName kind = 
  case kind of 
    Parenthesis    => "parenthesis"
    CurlyBrackets  => "curly bracket"
    SquareBrackets => "square bracket" 


expectMessage : (List String) -> String -> String 
expectMessage fst snd = concat ["Expected ", concat fst, " but instead got \"", snd, "\""]

getExpectTypeMessage : ExpectType -> Tkn -> String 
getExpectTypeMessage expect actual =
  case expect of 
    ExpectTkn tkn     => expectMessage ["a token like \"", show @{real} tkn, "\""] (show @{real} actual)
    ExpectKeyword str => expectMessage ["the keyword '", str, "'"] (show @{real} actual)
    ExpectId  => expectMessage ["an identifier (name for something)"] (show @{real} actual)
    ExpectStr => expectMessage ["a text (string)"] (show @{real} actual)
    ExpectInt => expectMessage ["a integer number"] (show @{real} actual)
    Unknown   => concat ["Unexpected ", (show @{real} actual)]
    TknEOF    => concat ["Unexpected token \"", (show @{real} actual), "\" try to add some top level structure (link here) instead of this."]

-- Reader error messages 

getReaderError : ErrInfo -> Range -> ReaderError -> String 
getReaderError err range (NotClosed kind) =  
  let kindName = getBetweenTypeName kind in
  commonErrorMessage err (Just range) ("You forgot to close the " ++ kindName ++ " that starts in the indicated place")

getReaderError err range (Expected expect tkn) = 
  let message = getExpectTypeMessage expect tkn in 
  commonErrorMessage err (Just range) message
  


-- The entry point of the error messages

getMessage : ErrInfo -> ErrorType -> String 
getMessage errInfo (LexicalError range err) = getLexerError errInfo range err
getMessage errInfo (ReadingError range err) = getReaderError errInfo range err
getMessage errInfo _ = commonErrorMessage errInfo Nothing "We dont have a good error message for it now lol sorry"

public export
Show ErrorData where
  show (MkErrorData file source error) = 
    case (getRange error) of 
      Just range@(MkRange (MkLoc c mainLine) _) => 
        let lined = forget $ lines source
            errRange = getErrorLineRange lined range
            line = (getIndex (cast mainLine) lined "")
            info = MKRangedErrInfo lined line file errRange range
        in 
          getMessage info error 
      Nothing => "Cannot process now"
