module Main 

import Syntax.Lexer
import Loc 

import Syntax.Parser 

str : String
str = """
      (data (Maybe a)
        (Just a)
        (Nothing))

      ; Records are like structs, they store multiple data at once and create some functions that can access that data inside User namespace.
      (record User
        (name String)
        (age Int))

      ; Algebraic effects and effect handlers will alsobe available to isolate kinds of side effects and improve the handling of them.
      (effect IO
        (print (-> String Unit)))


      ; Docstrings that are required to programs that
      (doc main \"The entrypoint of the program\")
      (defn main (-> (List String) (IO Unit)) [args]
        (case
          ((: arg1 (: arg2 tl)) (print (++ \"Hello :\" arg2)))
          (otherwise) (print \"Invalid argument, try again!\")))
      """ 

main : IO ()
main = print "Right"