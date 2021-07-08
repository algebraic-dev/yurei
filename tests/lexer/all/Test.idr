module Main

import Syntax.Lexer
import Syntax.Tokens
import Error 
import Loc

main : IO ()
main = case lex "(test \"test something\" 10 [1 2 3] {a b c d} (inside test \"kek\")) ;simple comment" of 
          Right res => putStr (concatMap ((++ "\n") . show) res) 
          Left _ => putStrLn "Error"