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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
    return $ Atom atom
    {-    "#t"        -> Bool True
    --    "#f"        -> Bool False
        '#':'x':n   -> Number $ fst $ head $ readHex n
        '#':'d':n   -> Number $ fst $ head $ readDec n
        '#':'o':n   -> Number $ fst $ head $ readOct n
        '#':'b':n   -> Number $ fst $ head $ readBin n
        _    -> Atom atom
    -}

parseBool::Parser LispVal
parseBool = do
    char '#'
    v <- oneOf "tf"
    return $ case v of
        't' -> Bool True
        'f' -> Bool False
        _   -> undefined

parseExpr :: Parser LispVal
parseExpr = 
    parseAtom
    <|> parseString
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBool
    <|> try parseChar
    <|> parseQuoted
    <|> do
        char '('
-- ok kurwa trzeba jeden jebnąć elegancko >>=
-- parseHexadecimal :: Parser LispVal
-- parseHexadecimal = do 
--     char '#'
--     char 'x'
--     x <- many1 $ oneOf "0123456789abcdef"
--     return $ Number $ fst $ head $ readHex x
        x <- try parseList <|> parseDottedList
        char ')'
        return x

parseChar:: Parser LispVal
parseChar = do
    try $ string "#\\"
    value <- try (string "space" <|> string "newline" ) <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
        "space"     -> ' '
        "newline"   -> '\n'
        _ -> head value

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
parseNumber = parseImplicitDecimal
    <|> parseExplicitDecimal
    <|> parseHexadecimal
    <|> parseOctal
    <|> parseBinary

parseImplicitDecimal :: Parser LispVal
parseImplicitDecimal = do
    number <- many1 digit
    let y = (Number . read) number
    return y

parseExplicitDecimal:: Parser LispVal
parseExplicitDecimal = do
    try $ string "#d"
    d <- many1 digit
    return $ Number $ read d

parseOctal:: Parser LispVal
parseOctal = do
    try $ string "#o"
    x <- many1 $ oneOf "01234567"
    return $  Number $ fst $ head $ readOct x

parseBinary:: Parser LispVal
parseBinary = do
    try $ string "#b"
    x <- many1 $ oneOf "10"
    return $ Number $ fst $ head $ readBin x

parseHexadecimal :: Parser LispVal
parseHexadecimal =
    try $ string "#x" >>
    many1 ( oneOf "0123456789abcdef") >>= \s -> return $ Number $ fst $ head $ readHex s

parseFloat :: Parser LispVal
parseFloat = do
    whole <- many1 digit
    char '.'
    frac <- many1 digit
    let y = (Float . read) $ whole <> ['.'] <> frac
    return  y

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList ::Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
