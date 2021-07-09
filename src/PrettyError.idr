module PrettyError

import Syntax.Lexer
import Loc
import Syntax.Tokens 
import Data.String.Extra
import Data.String
import Data.List 
import Data.List1
import ANSI
import Error 

getRange : ErrorType -> Maybe Range 
getRange (LexicalError  r _) = Just r 
getRange (ReadingError  r _) = Just r
getRange _                   = Nothing

space : Nat -> String 
space n = replicate n ' '

breakText : (Int, Int) -> String -> (String, String, String)
breakText (startColumn, endColumn) text = 
    let start  = strSubstr 0 (cast startColumn) text
        middle = strSubstr (cast startColumn) (cast $ endColumn - startColumn) text
        end    = strSubstr (cast endColumn)   (cast $ (cast $ length text) - endColumn) text in 
        (start, middle, end)

getIndex : Nat -> List a -> a -> a 
getIndex idx ls alt = 
  case (drop idx ls) of 
    h::tl => h 
    _ => alt

-- Simple Syntax Highlight based on the lexer

keywords : List String 
keywords = ["data", "defn", "doc", "case", "if", "else"]

specialWords : List String 
specialWords = ["->", "IO", "List"]

getTknColor : Tkn -> String 
getTknColor (TknNum     _) = "\x1b[32m" 
getTknColor (TknStr     _) = "\x1b[33m"  
getTknColor (TknComment _) = "\x1b[90m" 
getTknColor (TknId n) = 
  case find (== n) keywords of 
    Just _ => "\x1b[31m" 
    _ => case find (== n) specialWords of 
      Just _ => "\x1b[35m"
      _ => ""

getTknColor TknRSquare = "\x1b[34m" 
getTknColor TknLSquare = "\x1b[34m" 
getTknColor _ = "" 

syntaxHighlight : String -> String -> String 
syntaxHighlight style text = 
  case lex text of
    Right tokens => 
      snd $ foldl 
              (\cur => syntaxHighlightSingle (fst cur) (snd cur))
              (0, text)
              tokens 
    Left _ => text
  where 
    syntaxHighlightSingle : Int -> String -> (Range, Tkn) -> (Int, String) 
    syntaxHighlightSingle accPos text (MkRange start end, tkn) = 
      let lineEnd = if (Loc.line start) /= (Loc.line end) then length text else (cast $ Loc.line end) 
          pos = ((Loc.column start) + accPos, (Loc.column end) + accPos)
          (start, middle, end) = breakText pos text
          color = getTknColor tkn 
          newPos = (length color) + (length style) + 4 in 
      ((cast newPos) + accPos, concat [start, color, style, middle, Deco.reset, end, Deco.reset])

-- Formats the code 

getErrorLineRange : (List String) -> Range -> (Int, Int)
getErrorLineRange lines (MkRange (MkLoc _ startLine) (MkLoc _ endLine)) =   
  (if startLine - 2 < 0                    then 0           else startLine - 2,
   if endLine + 3 < (cast $ length lines)  then endLine + 3 else (cast $ length lines))

formatLineNumber : String -> Int -> String 
formatLineNumber text num = 
  concat [ justifyRight 6 ' ' (cast $ num + 1)
         , " | "]

formatCommonCodeLine : String -> String -> Int -> String
formatCommonCodeLine style text lineNum =
  concat [ style
         , formatLineNumber text lineNum
         , syntaxHighlight style text
         , Deco.reset]

createMarkLine : String -> Int -> Range -> String 
createMarkLine style textEnd (MkRange startLoc endLoc) =
  let start = Loc.column startLoc
      end = if (Loc.line startLoc) == (Loc.line endLoc)
              then Loc.column endLoc
              else textEnd
  in
  concat [ Deco.bold, style, space (cast start + 9), replicate (cast $ end - start) '^', Deco.reset ]

formatHighlightedCodeLine : Range -> String -> String -> Int -> String 
formatHighlightedCodeLine (MkRange start end) style text lineNum = 
  let (start, middle, end) = breakText ((Loc.column start), (Loc.column end)) text in
  concat [ formatLineNumber text lineNum
         , syntaxHighlight "" start
         , Deco.bold
         , style
         , middle
         , Deco.reset
         , syntaxHighlight "" end
         , Deco.reset ]

formatLines : Bool -> Range -> (Int,Int) -> Int -> (List String) -> String
formatLines markLine range (start, end) mainLine = 
    unlines 
    . map  formatLine
    . zip  [start..end] 
    . take (cast (end - start))
    . drop (cast start)
  where 
    formatLine : (Int, String) -> String 
    formatLine (line, text) =
      let marked = if markLine && line == mainLine + 1
                    then createMarkLine Fg.red (cast $ length text) range ++ "\n"
                    else ""  
          line   = if mainLine /= line 
                    then formatCommonCodeLine Deco.dim text line
                    else formatHighlightedCodeLine range Fg.red text line
      in 
      marked ++ line
-- Formats the header messages

getLexerErrorHeader : String -> Range -> LexerError -> String 

getLexerErrorHeader input range UnterminatedString = 
  "You forgot to terminate this string with another double quote!"

getLexerErrorHeader input range UnexpectedChar =
  "You cannot use this character here!"

getHeaderMessage : String -> ErrorType -> String 
getHeaderMessage input (LexicalError range err) = 
  getLexerErrorHeader input range err 

getHeaderMessage input _ = ""
 
formatHeader : String -> String    
formatHeader header = 
    let  (title ::: tl) = lines header in
      concat ["\n", errorTag, " ", title, "\n", unlines $ map ((space 9) ++) tl]
    where 
      errorTag : String 
      errorTag = concat [space 2, Bg.red, " ERROR ", Deco.reset]

fileHeader : Range -> String -> String 
fileHeader (MkRange (MkLoc col line) _) file = 
  let text = 
    concat ["on ./"
           , file, ":"
           , (cast (line+1))
           , ":",
            (cast (col + 1))] in
  concat [ space 4
         , Deco.dim
         , Fg.cyan
         , text
         , Deco.reset
         , "\n"] 

public export
Show ErrorData where
  show (MkErrorData file source error) = 
    case (getRange error) of 
      Just range@(MkRange fst@(MkLoc c mainLine) _) => 
        let lined = forget $ lines source
            errRange = getErrorLineRange lined range
            line = (getIndex (cast mainLine) lined "")
            headerMessage = getHeaderMessage line error
        in 
          unlines
            [ formatHeader headerMessage
            , fileHeader range file
            , formatLines True range errRange mainLine lined]
      Nothing => "Cannot process now"
