module Main

import Syntax.Lexer
import Syntax.Tokens
import System.File
import Error 
import Loc

main : IO ()
main = do 
    readFile "input" >>= \res =>
      case res of 
        Right input => 
          case lex input of 
                Right res => putStr (concatMap ((++ "\n") . show) res) 
                Left _ => putStrLn "Error"
        Left _ => print "File not found"