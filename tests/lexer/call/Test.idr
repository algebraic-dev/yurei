module Main

import Syntax.Lexer
import Syntax.Tokens
import Error 
import Loc

main : IO ()
main = case lex "(test \"test something\" 10)" of 
          Right res => putStr (concatMap ((++ "\n") . show) res) 
          Left _ => putStrLn "Error"