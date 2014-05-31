module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    --putStrLn . show $ (read (args !! 0) :: Float) + (read (args !! 1) :: Float)
    putStrLn $ readExpr $ args !! 0
    
    
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val