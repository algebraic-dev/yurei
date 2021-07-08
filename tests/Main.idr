module Main 

import Test.Golden

prefixFolder : String -> List String -> List String 
prefixFolder pref paths =
  map ((pref ++ "/") ++) paths

lexerTests : TestPool
lexerTests = MkTestPool "Lexer Tests" [] Nothing $ prefixFolder "lexer" 
  ["call", "all", "readme"]

main : IO ()
main = runner $ [ lexerTests ]