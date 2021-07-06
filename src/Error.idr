module Error 

import Loc

data GrammarError
  = NotClosedParenthesis 

data TypeError
  = LexicalError Loc
  | ParsingError Range GrammarError