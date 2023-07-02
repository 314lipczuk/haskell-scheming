module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric

{-

Todos, or things to implement to have it be a proper version of scheme
* Full numeric tower of types (short, single, double, long, complex, real, rational)

-}

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
    Right val -> "Found value" ++ show val

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Float Float
    | Character Char
    | String String
    | Bool Bool
    deriving (Show)

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many ( letter <|> symbol <|> digit )
    let atom = first:rest
    return $ case atom of
        "#t"        -> Bool True
        "#f"        -> Bool False
        '#':'x':n   -> Number $ fst $ head $ readHex n
        '#':'d':n   -> Number $ fst $ head $ readDec n
        '#':'o':n   -> Number $ fst $ head $ readOct n
        '#':'b':n   -> Number $ fst $ head $ readBin n
        _    -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseChar <|> parseAtom <|> parseString <|> parseFloat <|>  parseNumber

parseChar:: Parser LispVal
parseChar = do
    char '\''
    Character <$> letter

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

parseString :: Parser LispVal
parseString = do
    char '"'
    s <- many $  escapeChars <|> noneOf "\""
    char '"'
    return $ String s

parseNumber :: Parser LispVal
parseNumber = do
    number <- many1 digit
    let y = (Number . read) number
    return y

parseFloat :: Parser LispVal
parseFloat = do
    whole <- many1 digit
    char '.'
    frac <- many1 digit
    let y = (Float . read) $ whole <> ['.'] <> frac
    return  y
