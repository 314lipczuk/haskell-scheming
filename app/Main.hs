module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match" ++ show err
    Right val -> "Found value"


data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

--parseString :: Parser LispVal
--parseString = do
--    char '"'
--    x <- many $ noneOf "\""
--    char '"'
--    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many ( letter <|> symbol <|> digit )
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseExpr :: Parser LispVal
parseExpr =  parseAtom <|> parseString <|> parseNumber


--parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit

-- Exercises
-- 1. Rewrite parseNumber without liftM, using:
--      1. do-notation
--      2. >>=

-- rewritten with do-notation
--parseNumber :: Parser LispVal
--parseNumber = do
--    x <- many1 digit
--    let y = (Number . read) x
--    return  y

-- with >>= (monadic bind)
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \s -> return (Number $ read s)
-- return, bo return działa na wartości -> liftM działa na funkcje

-- Exercise 2
-- Make strings R5RS compliant (add escaping)

escapeChars :: Parser Char
escapeChars = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '\"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
{- 
This works by matching everything that starts with \ char,
then taking next character (x) and returning appropriate result
-}



parseString :: Parser LispVal
parseString = do
    char '"'
    s <- many $  escapeChars <|> noneOf "\"" 
    char '"'
    return $ String s