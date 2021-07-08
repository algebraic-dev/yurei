module Main 

import Syntax.Lexer
import Syntax.Reader
import Syntax.Parser
import Syntax.Tokens
import Error
import Data.List
import Loc 
import System.File

main : IO ()
main = print "Hello, World"