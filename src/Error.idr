module Error 

import Loc
import Syntax.Tokens 
import Data.String.Extra
import Data.String
import Data.List 
import Data.List1

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
data ErrorType
  = LexicalError Range
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

getErrorLineRange : (List String) -> Range -> (Int, Int)
getErrorLineRange lines (MkRange (MkLoc _ startLine) (MkLoc _ endLine)) =   
  (if startLine - 2 < 0                    then 0           else startLine - 2,
   if endLine + 2 < (cast $ length lines)  then endLine + 2 else (cast $ length lines) - 1)

resetANSI : String 
resetANSI = "\x1b[0m"

formatLine : String -> String -> Int -> String
formatLine style text lineNum =
   style ++ (justifyRight 4 ' ' (cast lineNum)) ++ " | " ++ text ++ resetANSI

formatHighlightedLine : Range -> String -> String -> Int -> String 
formatHighlightedLine (MkRange (MkLoc startColumn stLine) (MkLoc endColumn endLine)) style text lineNum = 
  if (stLine == endLine) then -- Too hard to highlight things that are not in the same line lol
    let start  = strSubstr 0 (cast startColumn) text
        middle = strSubstr (cast startColumn) (cast $ endColumn - startColumn) text
        end    = strSubstr (cast endColumn)   (cast $ (cast $ length text) - endColumn) text in 
    style 
    ++ (justifyRight 4 ' ' (cast lineNum)) ++ " | " 
    ++ start ++ "\x1b[5m" ++ middle ++ resetANSI ++ style ++ end 
    ++ resetANSI
  else 
    formatLine "\x1b[3m" text lineNum

formatNormalLine : Range -> Int -> (Int, String) -> String 
formatNormalLine range mainLine (line, text) = 
  if mainLine /= line then 
    formatLine "\x1b[2m" text line
  else 
    formatHighlightedLine range "" text line

formatLines : Range -> (Int,Int) -> Int -> (List String) -> String
formatLines range (start, end) mainLine rawLines = 
  concatMap ((++ " \n") . formatNormalLine range mainLine) $
  Data.Zippable.zip [start..end] $ 
  Data.List.take (cast (end - start)) $ 
  Data.List.drop (cast start) rawLines
  
getRange : ErrorType -> Maybe Range 
getRange (LexicalError  r  ) = Just r 
getRange (ReadingError  r _) = Just r
getRange _                   = Nothing


public export
Show ErrorData where
  show (MkErrorData file source error) = 
    case (getRange error) of 
      Just range@(MkRange fst@(MkLoc c mainLine) b) => 
        let lined = forget $ lines source
            errRange = getErrorLineRange lined range in 
        formatLines range errRange mainLine lined
      Nothing => "Cannot process now"
