module Main 

import Syntax.Lexer
import Syntax.Reader
import Syntax.Parser
import Syntax.Tokens
import Error.Data
import Error.Pretty
import Data.List
import Loc 
import System.File

compile : String -> String -> IO ()
compile fileName contents = 
  let res = (filter (not . isUseless . snd) <$> lex contents) >>= readToTerm in
  case res of 
    Left res => print $ (MkErrorData fileName contents res) 
    Right res => print "Compiled"

main : IO ()
main = do
  res <- readFile "teste.lsp"
  case res of 
    Left res  => print "File not found!" 
    Right res => compile "teste.lsp" res