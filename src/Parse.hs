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

predicateParser :: Parser (Pred String)
predicateParser = undefined

rtParser :: Parser (RefineType String)
rtParser = undefined

typeParser :: Parser (Type String)
typeParser = (Simple <$> (string "Int{" >> rtParser <* char '}'))

statementParser :: Parser (Statement String)
statementParser = many space >> undefined --string "TODO"

expressionParser :: Parser (Expr String)
expressionParser = many space >> undefined

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

parserMain fileName = do
    fileText <- readFile fileName 
    let parsed = parse funcParser "placeholder" fileText -- TODO: Replace placeholder with place to output errors?
    undefined -- TODO