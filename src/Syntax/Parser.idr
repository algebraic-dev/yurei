module Syntax.Parser 

import public Syntax.Term
import Syntax.Reader
import Error.Data 
import Data.String
import Control.Monad.Maybe
import Data.List1
import Data.List
import Loc  

Result : Type -> Type 
Result = Either (Range, ParserError)

-- Name checkers

checkFirstLetter : {m : String} -> (Char -> Bool) -> (m : AsList m)  -> Bool
checkFirstLetter fn Nil = False
checkFirstLetter fn (char :: tl) = fn char

isLowered : String -> Bool
isLowered m = checkFirstLetter isLower (asList m)

isCapitalized : String -> Bool
isCapitalized m = checkFirstLetter isUpper (asList m)

-- Helper functions

incrementRange : Range -> Int -> Range 
incrementRange (MkRange range _) len = (MkRange range (MkLoc ((Loc.column range) + len) (Loc.line range)))

-- Functions that actually parse IDS and paths. 

parsePathRaw : (String -> Either ParserError String) 
            -> (String -> Either ParserError String) 
            -> Range -> String -> Result Path

parsePathRaw checkMiddle checkLast range str = 
  parsePathWithPos 0 (forget $ split (== '.') str)
  where  
    parsePathWithPos : Int -> (List String) -> Result Path 
    parsePathWithPos curPos list =
      case list of 
        (start :: []) =>
          let finalRange = incrementRange range (cast $ length start) in
          case checkLast start of 
            Right res => pure $ PName finalRange res
            Left err  => Left $ (finalRange, err)
        (start :: tl) =>
          let finalRange = incrementRange range (cast $ length start) in
          case checkMiddle start of 
            Right res => do 
              resp <- parsePathWithPos ((cast $ length start) + 1) tl
              pure $ PDot range (MkName finalRange res) resp
            Left err => Left $ (finalRange, err)
        _ => Left $ (range, ImpossibleParsingError) 

parseTypePath : Range -> String -> Result Path 
parseTypePath =
  parsePathRaw
    (isCapitalStr CapitalModule) 
    (isCapitalStr CapitalType) 
  where 
    isCapitalStr : CapitalizedNameErr -> String -> Either ParserError String
    isCapitalStr err str = 
      if isCapitalized str
        then Right str 
        else Left (NeedCapitalizedName err)

parsePath : Range -> String -> Result Path 
parsePath = parsePathRaw (Right) (Right)

parseUnique : (String -> Bool) -> ParserError -> Range -> String -> Result Name 
parseUnique checkLast err range str = 
  let (hd ::: tl) = split (== '.') str in
  if (length tl) == 0
    then if (checkLast hd) then pure $ (MkName range hd) else Left (range, err)
    else Left $ (range, ExpectedIdButGotPath)

parseUniqueId : Range -> String -> Result Name
parseUniqueId = parseUnique isLowered NeedMinusculeName

parseUniqueType : Range -> String -> Result Name
parseUniqueType = parseUnique isCapitalized (NeedCapitalizedName CapitalType)

parseCapitalized : Range -> String -> Result Name
parseCapitalized = parseUnique isCapitalized (NeedCapitalizedName CapitalADTName)

unwrapId : TermExpr -> Result (Range, String)
unwrapId (TId range name) = Right (range, name)
unwrapId expr = Left (getRange expr, ExpectedIdentifier)

-- Parse everything

parseTypes : TermExpr -> Result Types
parseTypes (TList r (TId r_ "->" :: hd :: tl)) = do
  head <- parseTypes hd
  res <- traverse (\t => parseTypes t >>= \res => pure (getRange t, res)) tl 
  let list1 = (getRange hd, head) ::: res
  pure $ foldl (\acc, cur => TArrow (fst cur) (snd cur) acc) (snd $ last list1) (init list1)

parseTypes (TList r [TId range name, type]) = do 
  name <- parseTypePath range name 
  type <- parseTypes type 
  pure $ TPoly r name type

parseTypes (TId r str) = 
  if isCapitalized str 
    then pure $ TSimple r str 
    else pure $ TVar r str 

parseTypes expr = Left (getRange expr, NotAValidType)

parseLiteral : TermExpr -> Result Literal
parseLiteral (TStr r val) = pure (LStr r val)
parseLiteral (TInt r val) = pure (LInt r val)
parseLiteral expr  = Left (getRange expr, ExpectedLiteral) 

parseTypeDef : TermExpr -> Result (Name, Maybe Types)
parseTypeDef expr@(TId range name) = do 
  name <- parseUniqueType range name
  pure (name, Nothing)

parseTypeDef (TList range [name@(TId nrange str), types]) = do
  (name, _) <- parseTypeDef name
  type <- parseTypes types
  pure (name, Just type)

parseTypeDef (TList range (name :: types)) = Left (getRange name, ExpectedIdentifier)
parseTypeDef expr = Left (getRange expr, ExpectedTypeDef)

parsePat : TermExpr -> Result Pat
parsePat expr@(TStr r n) = PaLit r <$> parseLiteral expr
parsePat expr@(TInt r n) = PaLit r <$> parseLiteral expr
parsePat expr@(TId r name) = PaId <$> parseUniqueType r name
parsePat expr@(TList r (name :: tl)) =
  case name of 
    (TId nameRange "list") => PaList r <$> (traverse parsePat tl) 
    (TId range str) => do 
      path <- parseTypePath range str
      ls <- traverse parsePat tl
      pure $ PaData range path ls
    _ => Left (r, NotAValidPattern)

parsePat expr = Left (getRange expr, NotAValidPattern)

mutual 
  parseExprWithRange : TermExpr -> Result (Range, Expr)
  parseExprWithRange term = do
    res <- parseExpr term 
    pure (getRange term, res)

  parseExpr : TermExpr -> Result Expr

  parseExpr (TList range ((TId r_ "do") :: tl)) = do 
    body <- traverse parseExpr tl
    pure $ EDo range body

  parseExpr (TList range (TId r_ "lambda" :: hd :: tl)) = do 
    arg  <- unwrapId hd >>= uncurry parseUniqueId
    body <- traverse parseExpr tl 
    pure $ ELambda range arg (EDo range body)

  -- [a,b,c] ECall (ECall (a,b), c)
  parseExpr (TList range (fst :: params)) = do 
    fstP  <- parseExpr fst  
    body <- traverse (parseExprWithRange) params
    let result = foldl (\acc => \(range, expr) => ECall range acc expr) fstP body
    pure $ result 

  parseExpr (TId range name) = EId <$> parsePath range name
  parseExpr expr@(TInt range n) = (ELit range) <$> (parseLiteral expr)
  parseExpr expr@(TStr range n) = (ELit range) <$> (parseLiteral expr)

  parseExpr expr = Left $ (getRange expr, InvalidExpr)

-- Data def 

parseDataField : TermExpr -> Result (Name, Maybe Types)
parseDataField (TList range [TId ranged name, typed]) = do
    named <- parseCapitalized ranged name
    typed <- parseTypes typed
    pure $ (named, Just typed)

parseDataField (TList range [name@(TId r n)]) = parseDataField name
parseDataField (TId range name) = (\t => (t, Nothing)) <$> parseCapitalized range name
parseDataField expr = Left (getRange expr, ExpectedDataField)

parseDataDef : Range -> List TermExpr -> Result DataDef 
parseDataDef range (typeDef :: fields) = do 
  (name, types) <- parseTypeDef typeDef
  fields <- traverse parseDataField fields 
  pure $ DefData name types fields

parseDataDef range expr = Left $ (range, InvalidTopLevel "data")

-- Def 

parseDef : Range -> List TermExpr -> Result Def 
parseDef range [TId nameRange name, type, body] = do 
  named <- parseUniqueId nameRange name
  kind <- parseTypes type 
  value <- parseExpr body
  pure $ (Define named (Just kind) value)

parseDef range [TId nameRange name, body] = do 
  named <- parseUniqueId nameRange name
  value <- parseExpr body
  pure $ (Define named Nothing value)

parseDef range n = Left (range, InvalidTopLevel "def")

-- Defn 

parseDefFn : Range -> List TermExpr -> Result Def 
parseDefFn range ((TId nameRange name) :: type :: (TList argRange (TId r "list" :: args)) :: body) = do 
  named <- parseUniqueId nameRange name
  kind <- parseTypes type 
  bodyRes <- (EDo range) <$> traverse parseExpr body
  unwrapIds <- traverse (unwrapId) args
  args <- traverse (uncurry parseUniqueId) unwrapIds 
  let lambda = foldl (\acc, cur => ELambda range cur acc) (bodyRes) (reverse args)
  pure $ (Define named (Just kind) lambda)

parseDefFn range [TId nameRange name, body] = do 
  named <- parseUniqueId nameRange name
  value <- parseExpr body
  pure $ (Define named Nothing value)

parseDefFn range n = Left (range, InvalidTopLevel "defn")

parseRecordField : TermExpr -> Result (Name, Types)
parseRecordField (TList range [TId ranged name, typed]) = do
    named <- parseUniqueId ranged name
    typed <- parseTypes typed
    pure $ (named, typed)

parseRecordField expr = Left (getRange expr, NotAValidRecordField)

parseRecord : Range -> List TermExpr -> Result RecordDef
parseRecord range (typeDef :: fields) = do 
  (name, types) <- parseTypeDef typeDef
  fields <- traverse parseRecordField fields 
  pure $ DefRecord name types fields

parseRecord range expr = Left (range, InvalidTopLevel "record")

parseTopLevel : TermExpr -> Program -> Result Program 
parseTopLevel expr program =
  case expr of  
      TList range (TId dataName name :: tl) => 
        case name of 
          "data" => do 
            res <- parseDataDef range tl 
            pure $ (record { dataDefs $= ((::) res) } program)
          "defn" => do 
            res <- parseDefFn range tl 
            pure $ (record { defs $= ((::) res) } program)
          "def" => do 
            res <- parseDef range tl 
            pure $ (record { defs $= ((::) res) } program)
          "record" => do 
            res <- parseDef range tl 
            pure $ (record { defs $= ((::) res) } program)
          name => Left $ (dataName, InvalidTopLevel name)
      other => Left $ (getRange expr, Unreachable)

public export
parseProgram : String -> List TermExpr -> Either ErrorType Program 
parseProgram name exprs = 
  let program = MkProgram name [] [] in
  let res = foldl (\program, cur => program >>= (parseTopLevel cur)) (Right program) exprs in 
  case res of 
    Left (range, err) => Left $ ParsingError range err 
    Right res => Right res
  