module Syntax.Lexer

import Loc
import Text.Lexer
import Text.Lexer.Core

-- Just token definition

public export 
data Tkn 
  = TknId       String
  | TknNum      Int
  | TknStr      String
  | TknComment  String
  | TknLPar        
  | TknRPar       
  | TknRCurly      
  | TknLCurly     
  | TknRSquare    
  | TknLSquare    
  | TknWhitespace 
  | TknLB         
  | TknDot

public export
Show Tkn where
  show (TknId dt)      = "ID: " ++ show dt
  show (TknNum dt)     = "INT: " ++ show dt
  show (TknStr dt)     = "STR: " ++ show dt
  show (TknComment dt) = "COMMENT: " ++ show dt
  show TknLPar         = "\"(\""
  show TknRPar         = "\")\""
  show TknLCurly       = "\"{\""
  show TknRCurly       = "\"}\""
  show TknLSquare      = "\"[\""
  show TknRSquare      = "\"]\""
  show TknWhitespace   = "WHITESPACE"
  show TknLB           = "LB"
  show TknDot          = "\".\""

public export
Eq Tkn where 
  (==) (TknId a)  (TknId b)          = a == b
  (==) (TknNum a) (TknNum b)         = a == b
  (==) (TknStr a) (TknStr b)         = a == b
  (==) (TknComment a) (TknComment b) = a == b
  (==) TknLPar TknLPar             = True
  (==) TknRPar TknRPar             = True
  (==) TknRCurly TknRCurly         = True
  (==) TknLCurly TknLCurly         = True
  (==) TknRSquare TknRSquare       = True
  (==) TknLSquare TknLSquare       = True
  (==) TknWhitespace TknWhitespace = True
  (==) TknLB TknLB                 = True
  (==) TknDot TknDot               = True 
  (==) _ _ = False

public export
tokenName : Tkn -> String
tokenName (TknId _)      = "identifier"
tokenName (TknNum _)     = "int"
tokenName (TknStr _)      = "string"
tokenName (TknComment _) = "comment"
tokenName TknLPar        = "left parenthesis"
tokenName TknRPar        = "right parenthesis"
tokenName TknLCurly      = "left curly bracket"
tokenName TknRCurly      = "right curly bracket"
tokenName TknLSquare     = "left square bracket"
tokenName TknRSquare     = "right square bracket"
tokenName TknWhitespace  = "whitespace"
tokenName TknLB          = "line break"
tokenName TknDot         = "dot"

-- The entry point of lexing 

comment : Lexer
comment = (is ';') <+> some (pred (/= '\n')) <+> ((is '\n') <|> empty)

string : Lexer
string = (is '"') <+> some (pred (/= '"')) <+> (is '"')

id : Lexer
id = some (non (oneOf "; \n\r()[]{}"))

tokenMap : TokenMap Tkn
tokenMap = 
   [
    (oneOf "\n\r", const TknLB),
    (digits  , TknNum . cast),
    (id      , TknId),
    (is '('  , const TknLPar),
    (is ')'  , const TknRPar),
    (is '['  , const TknLSquare),
    (is ']'  , const TknLSquare),
    (is '{'  , const TknLCurly),
    (is '}'  , const TknRCurly),
    (space   , const TknWhitespace),
    (comment , TknComment),
    (string  , TknStr)]

tokenDataToLoc : TokenData x -> (Loc, x) 
tokenDataToLoc (MkToken col line c) = (MkLoc line col, c)

public export
lex : String -> Either Loc (List (Loc, Tkn))
lex str
  = case lex tokenMap str of
    (tokens, _, _,   "")   => pure $ map tokenDataToLoc tokens
    (_, line, column, _) => Left  $ MkLoc line column