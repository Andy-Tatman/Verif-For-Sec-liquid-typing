module Parse
( 
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
variableParser = (Variable <$> (many1 letter))

intParser :: Parser (Expr String)
intParser = do
    strN <- many1 digit
    return $ ConstI $ read strN


predicateParser :: Parser (Pred String)
predicateParser = try (Conj <$> (predicateParser) <*> ( many space >> string "&&" >> predicateParser) ) <|> 
            try (Disj <$> (predicateParser) <*> ( many space >> string "||" >> predicateParser) ) <|>
            try (Neg <$> (char '!' >> many space >> predicateParser)) <|>
            try (string "True" >> trueParser) <|>
                (string "False" >> falseParser) 

rtParser :: Parser (RefineType String)
rtParser = (Rt <$> (many1 letter) <*> (many space >> char '|' >> predicateParser <* many space))
    

typeParser :: Parser (Type String)
typeParser = (Simple <$> (string "Int{" >> rtParser <* char '}'))
    -- TODO: Functype?
    
statementParser :: Parser (Statement String)
statementParser = many space >> undefined --string "TODO"

expressionParser :: Parser (Expr String)
expressionParser = many space >> (
    undefined <|> -- BinOp
    try intParser 
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

parserMain :: String -> IO () -- TODO: Change output signature
parserMain fileName = do
    fileText <- readFile fileName 
    let parsed = parse funcParser "placeholder" fileText -- TODO: Replace placeholder with place to output errors?
    case parsed of
        Left x -> print x -- ERROR
        Right y -> print "YAY" -- Parsed correctly