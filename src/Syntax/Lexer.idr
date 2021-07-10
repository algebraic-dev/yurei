module Syntax.Lexer

import Loc
import Error.Data
import Text.Lexer
import Text.Lexer.Core
import Syntax.Tokens
import Data.String
import Data.String.Extra

-- The entry point of lexing 

comment : Lexer
comment = (is ';') <+> some (pred (/= '\n'))

string : Lexer
string = (is '"') <+> some (pred (\s => s /= '"' && s /= '\n')) <+> (is '"')

id : Lexer
id = some (non (oneOf "\"; \n\r()[]{}"))

trimStartEnd : Int -> Int -> String -> String
trimStartEnd start end text = strSubstr start ((cast $ length text) - (end + start)) text

tokenMap : TokenMap (Int, Tkn)
tokenMap = 
   map (\(rule, fun) => (rule, (\s => (cast $ length s, fun s)))) 
      [(oneOf "\n\r", const TknLB),
      (digits, TknNum . cast),
      (id    , TknId),
      (is '(', const TknLPar),
      (is ')', const TknRPar),
      (is '[', const TknLSquare),
      (is ']', const TknRSquare),
      (is '{', const TknLCurly),
      (is '}', const TknRCurly),
      (space, const TknWhitespace),
      (comment, TknComment . (trimStartEnd 1 0)),
      (string, TknStr . (trimStartEnd 1 1))]

-- It transforms a parser of Tkn to (Int, Tkn). The tkn parameter indicates the 
-- Length of the string that it's getting.

mapApply : (Lexer, String -> Tkn) -> (Lexer, String -> (Int, Tkn))
mapApply (rule, func) = (rule, \s => (cast $ length s, func s))

-- The token data for comments are broken. So think twice before using the range for
-- comments.

tokenDataToLoc : TokenData (Int, Tkn) -> (Range, Tkn) 
tokenDataToLoc (MkToken line col (len, tkn)) = 
  (MkRange (MkLoc {column = col, line = line}) (MkLoc {column = col + len, line }), tkn)

-- The main function that lexes the string to a list of (range,tkn)

public export
lex : String -> Either ErrorType (List (Range, Tkn))
lex str
  = case lex tokenMap str of
    (tokens, _, _,   "") => Right $ map tokenDataToLoc tokens
    (_, line, column, res) => Left $ LexicalError 
      (MkRange (MkLoc { column, line }) (MkLoc { column = column + 1, line }))
      (if (take 1 res) == "\"" then UnterminatedString else UnexpectedChar)

public export
isUseless : Tkn -> Bool
isUseless tkn = case tkn of 
                  TknComment x => True
                  TknWhitespace => True 
                  TknLB => True 
                  _ => False