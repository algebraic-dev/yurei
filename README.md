<p align="center"> 👻 </p>
<h2 align="center"> Yūrei </h2>
<p align="center"> Some lisp to study type checkers.</p>

## Objectives

- [ ] Lexer
- [ ] Parser
- [ ] Beautiful Error Messages
- [ ] Type Checking and Inference
- [ ] Effect handling
- [ ] Interpreter
- [ ] Defunctionalization and Lambda lifting
- [ ] Other optimizations
- [ ] Java Bytecode

## Language

```lisp
; Algebraic data type of sum, they're like enums but can hold data.
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
(doc main "The entrypoint of the program")
(defn main (-> (List String) (IO Unit)) [args]
  (case
    ((: arg1 (: arg2 tl)) (print (++ "Hello :" arg2)))
    (otherwise) (print "Invalid argument, try again!")))
```

## Features

- [ ] ADT
- [ ] Effect Handling
- [ ] Currying
- [ ] Type Inference
