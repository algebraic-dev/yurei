module Error 

import Loc
import Syntax.Tokens 

public export
data GrammarError
  = NotClosedParenthesis 
  | Expected Range Tkn Tkn
  | ExpectedEmpty Range Tkn Tkn
  | ExpectedKeyword Range String Tkn

public export
data TypeError
  = LexicalError Range
  | ParsingError Range GrammarError