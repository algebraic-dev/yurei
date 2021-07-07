module Syntax.Lexer

import Loc
import Error
import Text.Lexer
import Text.Lexer.Core
import Syntax.Tokens

-- The entry point of lexing 

comment : Lexer
comment = (is ';') <+> some (pred (/= '\n')) <+> ((is '\n') <|> empty)

string : Lexer
string = (is '"') <+> some (pred (/= '"')) <+> (is '"')

id : Lexer
id = some (non (oneOf "; \n\r()[]{}"))

tokenMap : TokenMap (Int, Tkn)
tokenMap = 
   map (\(rule, fun) => (rule, (\s => (cast $ length s, fun s)))) 
      [(oneOf "\n\r", const TknLB),
      (digits  , TknNum . cast),
      (id      , TknId),
      (is '('  , const TknLPar),
      (is ')'  , const TknRPar),
      (is '['  , const TknLSquare),
      (is ']'  , const TknRSquare),
      (is '{'  , const TknLCurly),
      (is '}'  , const TknRCurly),
      (space   , const TknWhitespace),
      (comment , TknComment),
      (string  , TknStr)]

-- It transforms a parser of Tkn to (Int, Tkn). The tkn parameter indicates the 
-- Length of the string that it's getting.

mapApply : (Lexer, String -> Tkn) -> (Lexer, String -> (Int, Tkn))
mapApply (rule, func) = (rule, \s => (cast $ length s, func s))

-- The token data for comments are broken. So think twice before using the range for
-- comments.

tokenDataToLoc : TokenData (Int, Tkn) -> (Range, Tkn) 
tokenDataToLoc (MkToken col line (len, tkn)) = (MkRange (MkLoc line col) (MkLoc line (col + len)), tkn)

-- The main function that lexes the string to a list of (range,tkn)

public export
lex : String -> Either ErrorType (List (Range, Tkn))
lex str
  = case lex tokenMap str of
    (tokens, _, _,   "") => Right $ map tokenDataToLoc tokens
    (_, line, column, _) => Left  $ LexicalError (MkRange 
                                                    (MkLoc line column) 
                                                    (MkLoc line (column+1)))

public export
isUseless : Tkn -> Bool
isUseless tkn = case tkn of 
                  TknComment x => True
                  TknWhitespace => True 
                  TknLB => True 
                  _ => False