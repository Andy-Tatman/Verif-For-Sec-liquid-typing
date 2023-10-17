module Parse
( 
    parserMain
    , parserTest1 
) where

import Expr
import Text.Parsec
import Text.Parsec.String

-- num :: Parser Integer
-- num = do
--     n <- many1 digit
--     return (read n)

-- boolParser :: Parser Bool
-- boolParser = (try (string "True" >> True) <|> (string "False" >> False))
-- boolParser = do
    -- (try (string "True"); return (ConstB True) ) <|> try (string "False"); return (ConstB False)
    -- resultT <- try (string "True")
    -- case resultT of 
    --     Left _ -> do -- Error
    --         resultF <- try (string "False")
    --         case resultF of
    --             Left x -> return x -- Error
    --             Right _ -> return (ConstB False) -- OK
    --     Right _ -> return (ConstB True) -- OK

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
-- Pred = Pred "&&" Pred | Pred "||" Pred | subPred
-- subPred = "(" Pred ")" | "!" Pred | CompOp ... | ConstB Bool
predicateParser :: Parser (Pred String)
predicateParser = many space >>
    (
    try (Conj <$> (predicateParser) <*> ( many space >> string "&&" >> predicateParser) ) <|> 
    try (Disj <$> (predicateParser) <*> ( many space >> string "||" >> predicateParser) ) <|>
    try (subPredParser)
    )

subPredParser :: Parser (Pred String)
subPredParser = many space >> 
    (
    try (char '(' >> many space >> predicateParser <* many space <* char ')') <|> 
    try (Neg <$> (char '!' >> many space >> predicateParser)) <|>
    -- CompOp
    try (CompOp (LEQ) <$> (expressionParser) <*> (many space >> string "<=" >> expressionParser)) <|>
    try (CompOp (LE) <$> (expressionParser) <*> (many space >> string "<" >> expressionParser))   <|>
    try (CompOp (GEQ) <$> (expressionParser) <*> (many space >> string ">=" >> expressionParser)) <|>
    try (CompOp (GE) <$> (expressionParser) <*> (many space >> string ">" >> expressionParser))   <|>
    try (CompOp (EQU) <$> (expressionParser) <*> (many space >> string "==" >> expressionParser)) <|>
    try (CompOp (NEQ) <$> (expressionParser) <*> (many space >> string "!=" >> expressionParser)) <|>
    try (string "True" >> trueParser) <|>
        (string "False" >> falseParser) 
    )


rtParser :: Parser (RefineType String)
rtParser = try (Rt <$> (many1 letter) <*> (many space >> char '|' >> predicateParser <* many space))
    

typeParser :: Parser (Type String)
typeParser = try (many space >> Simple <$> (string "Int{" >> rtParser <* char '}'))
    -- TODO: Functype?


-- Note: The ';' in the grammar of Statement is handled in the funcParser. (So not here!)
statementParser :: Parser (Statement String)
statementParser = many space >> (
    try (LetAssign <$> (string "let" >> many1 space >> many1 letter) <*> (many space >> char ':' >> typeParser) 
            <*> (many space >> char '=' >> many space >> expressionParser)) <|>
    try (Expr <$> expressionParser) 
    ) 


-- To enforce operator precedence, we split the Expr grammar up into 3:
-- Expr = Expr "+" subExpr | Expr "-" subExpr | subExpr
-- subExpr = "-" Expr | subExpr "*" atom | subExpr "/" atom | subExpr "%" atom | atom
-- atom = Const Int | Variable | "(" Expr ")"
expressionParser :: Parser (Expr String)
expressionParser = many space >> (
    -- BinOp:
    try (BinOp (Add) <$>  (expressionParser) <*> (many space >> char '+' >> subExprParser)  ) <|> -- + (Add)
    try (BinOp (Sub) <$> (expressionParser) <*> (many space >> char '-' >> subExprParser)  ) <|> -- - (Sub)
    subExprParser
    )


subExprParser :: Parser (Expr String)
subExprParser = many space >> (
    try (Minus <$> (char '-' >> many space >> expressionParser) ) <|> -- UnaryMinus
    --BinOp:
    try (BinOp (Mul) <$> (expressionParser) <*> (many space >> char '*' >> subExprParser)  ) <|> -- * (Mul)
    try (BinOp (Div) <$> (expressionParser) <*> (many space >> char '/' >> subExprParser)  ) <|> -- / (Div)
    try (BinOp (Mod) <$> (expressionParser) <*> (many space >> char '%' >> subExprParser)  ) -- % (Mod)
    
    )

atomExprParser :: Parser (Expr String)
atomExprParser = many space >> (
    try (char '(' >> many space >> expressionParser <* many space <* char ')') <|> -- (Expr)
    try (variableParser) <|> -- Var
    try intParser -- Int
    )

-- -- Handles the last part of the function, which is a statement that specifically must be an expression.
-- returnParser :: Parser (Statement String)
-- returnParser = (Expr <$> (expressionParser))

-- varDeclParser :: Parser (Expr String, Type String)
-- varDeclParser = undefined


funcParser :: Parser (Function String)
funcParser = spaces >> 
    (Func 
    <$> (string "/*" >> many space >> many1 letter) -- fvar
    <*> (char ':' >> typeParser) -- fpre
    <*> (many space >> string "=>" >> many space >> typeParser <* string "*/") -- Post type
    <*> (many space >> string "main" >> many space >> char '=' >> many space >> char '\\' 
            >> many1 letter <* char '.' ) -- fBound
    <*> (char '{' >> many (statementParser <* char ';') ) -- Body
    <*> (expressionParser <* char '}')
       
    ) <* eof


parserTest1 :: String -> IO ()
parserTest1 fileName = do
    fileText <- readFile fileName 
    let parsed = parse funcParser "placeholder" fileText -- TODO: Replace placeholder with place to output errors?
    case parsed of
        Left x -> print x -- ERROR
        Right y -> do 
            print "YAY" -- Parsed correctly
            print y
            

parserMain :: String -> IO () -- TODO: Change output signature
parserMain fileName = do
    fileText <- readFile fileName 
    let parsed = parse funcParser "placeholder" fileText -- TODO: Replace placeholder with place to output errors?
    print "Exit parse"
    case parsed of
        Left x -> do 
            print "ERROR"
            print x -- ERROR
        Right y -> do 
            print "YAY" -- Parsed correctly
            print y
    print "End parser Main"