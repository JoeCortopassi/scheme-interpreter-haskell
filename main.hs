module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
instance Show LispVal where show = showVal

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr $ args !! 0
    
    
data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


escapedChars :: Parser Char
escapedChars = do char '\\' 
                  x <- oneOf "\\\"nrt" 
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
    
    
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
    
    
-- parsers
parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x
    
              
                
parseAtom :: Parser LispVal
parseAtom  = do first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest -- same as [first] ++ rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x
                                                        

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \x -> (return . Number .read) x 
{-
parseNumber = liftM (Number . read) $ many1 digit                       This is equivalent to...
parseNumber = do x <- many1 digit                                       ...this example and...
                (return . Number . read) x
parseNumber = many1 digit >>= \x -> (return . Number .read) x           ...this example
-}      
                

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
    

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]
    
    



