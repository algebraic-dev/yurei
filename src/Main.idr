module Main 

import Error.Data
import Error.Pretty

import Syntax.Lexer
import Syntax.Reader
import Syntax.Parser
import Syntax.Tokens
import Syntax.Term

import Typing.Infer

import Data.List
import Loc 
import System.File

import Control.App
import Control.App.Console
import Control.App.FileIO

compile : String -> String -> IO ()
compile fileName contents = 
  let res = (filter (not . isUseless . snd) <$> lex contents) 
              >>= readToTerm
              >>= (parseProgram "teste.lsp") in
  case res of 
    Left res => print $ (MkErrorData fileName contents res) 
    Right (MkProgram s t d r) => 
      let res = traverse runInference d in 
      case res of 
        Right e => print (map snd e) 
        Left e  => print e

main : IO ()
main = do
  res <- readFile "teste.lsp"
  case res of 
    Left res  => print "File not found!" 
    Right res => compile "teste.lsp" res