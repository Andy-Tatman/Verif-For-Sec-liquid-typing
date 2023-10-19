module Parse
  ( 
  parserMain
  ,   parserTest1 
  ) where

import Expr
import Text.Parsec
import Text.Parsec.String


trueParser :: Parser (Pred String)
trueParser = return (ConstB True)

falseParser :: Parser (Pred String)
falseParser = return (ConstB False)

variableParser :: Parser (Expr String) -- Returns a variable
variableParser = try (Variable <$> (many1 letter))

intParser :: Parser (Expr String)
intParser = do
    strN <- many1 digit
    return $ ConstI $ read strN

-- To enforce operator precedence, we split the Pred grammar up into 2:
-- Pred = Pred "&&" subPred | Pred "||" subPred | subPred
-- subPred = "(" Pred ")" | "!" Pred | CompOp ... | ConstB Bool | Expr CompOp Expr
-- (with CompOp \in {<=, <, >=, >, ==, !=}.)
-- 
-- In order to avoid Left-recursion, we then transform the Pred grammar into the following: 
--   (We denote the EMPTY STRING with "e".)
-- Pred = subPred Pred'
--  Pred' = "&&" subPred Pred' | "||" subPred Pred' | e
-- subPred = "(" Pred ")" | "!" Pred | CompOp ... | ConstB Bool | Expr CompOp Expr
--  (We have implemented the parser slightly differently to this grammar, to accommodate Haskell.)
predicateParser :: Parser (Pred String)
predicateParser = many space >> 
    (
       try (Conj <$> (subPredParser) <*> ( many space >> string "&&" >> predicateParser_Accent) ) <|> 
       try (Disj <$> (subPredParser) <*> ( many space >> string "||" >> predicateParser_Accent) ) <|>
       subPredParser
    )

predicateParser_Accent :: Parser (Pred String)
predicateParser_Accent = many space >> 
    (
       try (Conj <$> (subPredParser) <*> ( many space >> string "&&" >> predicateParser_Accent) ) <|> 
       try (Disj <$> (subPredParser) <*> ( many space >> string "||" >> predicateParser_Accent) ) <|>
       subPredParser
    )

subPredParser :: Parser (Pred String)
subPredParser = many space >> 
    (
    try (char '(' >> many space >> predicateParser <* many space <* char ')') <|> 
    try (Neg <$> (char '!' >> many space >> predicateParser)) <|>
    try (string "True" >> trueParser)   <|>
    try (string "False" >> falseParser) <|> 
    -- -- CompOp
    -- try (CompOp (LEQ) <$> (expressionParser) <*> (many space >> string "<=" >> expressionParser)) <|>
    -- try (CompOp (LE) <$> (expressionParser) <*> (many space >> string "<" >> expressionParser))   <|>
    -- try (CompOp (GEQ) <$> (expressionParser) <*> (many space >> string ">=" >> expressionParser)) <|>
    -- try (CompOp (GE) <$> (expressionParser) <*> (many space >> string ">" >> expressionParser))   <|>
    -- try (CompOp (EQU) <$> (expressionParser) <*> (many space >> string "==" >> expressionParser)) <|>
    -- try (CompOp (NEQ) <$> (expressionParser) <*> (many space >> string "!=" >> expressionParser)) 
    try (Comp <$> comparisonParser)
    )

-- Attempts to parse a comparison (expr1 COMPOP expr2), where COMPOP = <, <=, >, >=, == or !=.
-- WARNING: This function will consume the input for expr1 if possible, even if 
--          the overall comparison does not parse. Call this function with a try!
comparisonParser :: Parser (Comparison String)
comparisonParser = do
    -- do firstExpr separately first, for efficiency
    let firstExpr = try (expressionParser)
    try (Compar (LEQ) <$> firstExpr <*> (many space >> string "<=" >> expressionParser)) <|>
        try (Compar (LE) <$> firstExpr <*> (many space >> string "<" >> expressionParser)) <|>
        try (Compar (GEQ) <$> firstExpr <*> (many space >> string ">=" >> expressionParser)) <|>
        try (Compar (GE) <$> firstExpr <*> (many space >> string ">" >> expressionParser)) <|>
        try (Compar (EQU) <$> firstExpr <*> (many space >> string "==" >> expressionParser)) <|>
        try (Compar (NEQ) <$> firstExpr <*> (many space >> string "!=" >> expressionParser)) 


rtParser :: Parser (RefineType String)
rtParser = try (Rt <$> (many space >> many1 letter) <*> (many space >> char '|' >> predicateParser <* many space))
    

typeParser :: Parser (Type String)
typeParser = try (many space >> Simple <$> (string "Int{" >> rtParser <* char '}'))
    -- TODO: Functype?


-- Attempts to parse a single statement. If not possible, this function will not 
-- consume any input (aside from whitespace).
-- Note: The ';' in the grammar of Statement is handled in the funcParser. (So not here!)
statementParser :: Parser (Statement String)
statementParser = many space >> (
    try (LetAssign <$> (string "let" >> many1 space >> many1 letter) <*> (many space >> char ':' >> typeParser) 
            <*> (many space >> char '=' >> many space >> expressionParser)) <|>
    try (Expr <$> expressionParser) 
    ) 


-- To enforce operator precedence, we split the Expr grammar up into 3:
-- Expr = Expr "+" subExpr | Expr "-" subExpr | subExpr
-- subExpr = subExpr "*" atom | subExpr "/" atom | subExpr "%" atom | atom
-- atom = Const Int | Variable | "(" Expr ")" | "-" Expr
--
-- In order to avoid Left-recursion, we then transform the Expr grammar into the following: 
--   (We denote the EMPTY STRING with "e".)
-- Expr = subExpr Expr'
--  Expr' = "+" subExpr Expr' | "-" subExpr Expr' | e
-- subExpr = atom subExpr'
--  subExpr' = "*" atom subExpr' | "/" atom subExpr' | "%" atom subExpr' | e
-- atom = Const Int | Variable | "(" Expr ")" | "-" Expr
--  (We have implemented the parser slightly differently to this grammar, to accommodate Haskell.)
expressionParser :: Parser (Expr String)
expressionParser = many space >> (
    try (BinOp (Add) <$> (subExprParser) <*> (many space >> char '+' >> expressionParser_Accent)  ) <|> -- + (Add)
    try (BinOp (Sub) <$> (subExprParser) <*> (many space >> char '-' >> expressionParser_Accent)  ) <|> -- - (Sub)
    subExprParser
    )

expressionParser_Accent :: Parser (Expr String)
expressionParser_Accent = many space >> (
    try (BinOp (Add) <$> (subExprParser) <*> (many space >> char '+' >> expressionParser_Accent)  ) <|> -- + (Add)
    try (BinOp (Sub) <$> (subExprParser) <*> (many space >> char '-' >> expressionParser_Accent)  ) <|> -- - (Sub)
    subExprParser
    )    

subExprParser :: Parser (Expr String)
subExprParser = many space >> (
    --BinOp:
    try (BinOp (Mul) <$> (atomExprParser) <*> (many space >> char '*' >> subExprParser_Accent)  ) <|> -- * (Mul)
    try (BinOp (Div) <$> (atomExprParser) <*> (many space >> char '/' >> subExprParser_Accent)  ) <|> -- / (Div)
    try (BinOp (Mod) <$> (atomExprParser) <*> (many space >> char '%' >> subExprParser_Accent)  ) <|> -- % (Mod)
    atomExprParser
    )

subExprParser_Accent :: Parser (Expr String)
subExprParser_Accent = many space >> (
    try (BinOp (Mul) <$> (atomExprParser) <*> (many space >> char '*' >> subExprParser_Accent)  ) <|> -- * (Mul)
    try (BinOp (Div) <$> (atomExprParser) <*> (many space >> char '/' >> subExprParser_Accent)  ) <|> -- / (Div)
    try (BinOp (Mod) <$> (atomExprParser) <*> (many space >> char '%' >> subExprParser_Accent)  ) <|> -- % (Mod)
    atomExprParser
    )

atomExprParser :: Parser (Expr String)
atomExprParser = many space >> (
    try (char '(' >> many space >> expressionParser <* many space <* char ')') <|> -- (Expr)
    try (Minus <$> (char '-' >> many space >> expressionParser) ) <|> -- UnaryMinus
    try (variableParser) <|> -- Var
    try intParser -- Int
    )

-- -- Handles the last part of the function, which is a statement that specifically must be an expression.
-- returnParser :: Parser (Statement String)
-- returnParser = (Expr <$> (expressionParser))

-- Parses the main function
funcParser :: Parser (Function String)
funcParser = spaces >> 
    (Func 
    <$> (string "/*" >> many space >> many1 letter) -- fvar
    <*> (char ':' >> typeParser) -- fpre
    <*> (many space >> string "=>" >> many space >> typeParser <* many space <* string "*/") -- Post type
    <*> (many space >> string "main" >> many space >> char '=' >> many space >> char '\\' 
            >> many1 letter <* char '.' ) -- fbound
    <*> (many space >> char '{' >> many ( try (statementParser <* many space <* char ';') ) ) -- fbody
    <*> (many space >> expressionParser <* many space <* char '}') -- fret

    -- <*> (many space >>  char '{' >> expressionParser <* many space <* char '}') -- (Test line.)
       
    ) <* many space <* eof


parserTest1 :: String -> IO ()
parserTest1 fileName = do
    fileText <- readFile fileName 
    let parsed = parse funcParser "" fileText -- TODO: Replace placeholder with place to output errors?
    case parsed of
        Left x -> print x -- ERROR
        Right y -> do 
            print "YAY" -- Parsed correctly
            print y
            
parserMain :: String -> Either ParseError (Function String)
parserMain fileText = parse funcParser "" fileText
-- parserMain :: String -> Maybe (Function String) -- TODO: Change output signature
-- parserMain fileText = do
--     let parsed = parse funcParser "placeholder" fileText
--     case parsed of
--         Left _ -> do -- ERROR
--             Nothing
--         Right y -> do -- Parsed correctly
--             Just y

    -- fileText <- readFile fileName 
    -- let parsed = parse funcParser "placeholder" fileText -- TODO: Replace placeholder with place to output errors?
    -- print "Exit parse"
    -- case parsed of
    --     Left x -> do 
    --         print "ERROR"
    --         print x -- ERROR
    --     Right y -> do 
    --         print "YAY" -- Parsed correctly
    --         print y
    -- print "End parser Main"