import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char

import Control.Monad.Combinators.Expr
import Control.Monad
import Control.Applicative hiding (many)
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L

import System.IO

type Parser = Parsec Void String
sc :: Parser ()
sc = L.space space1 empty empty 
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

data Expr
  = Num Int
  | Plus Expr Expr
  | Mult Expr Expr
    deriving (Show) 

numP :: Parser Expr
numP = Num <$> integer 

termP :: [[Operator Parser Expr]] -> Parser Expr
termP t = parens (exprP t) <|> numP

exprP :: [[Operator Parser Expr]] -> Parser Expr
exprP operatorTable =
    makeExprParser (termP operatorTable) operatorTable

operatorTable1 :: [[Operator Parser Expr]]
operatorTable1 =
  [
   [binary "+" Plus, binary "*" Mult]
  ]

operatorTable2 :: [[Operator Parser Expr]]
operatorTable2 =
  [
   [binary "+" Plus],
   [binary "*" Mult]
  ]

binary :: String -> (Expr -> Expr -> Expr)
       -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

eval :: Expr -> Int
eval (Num n) = n
eval (Plus n1 n2) = eval n1 + (eval n2)
eval (Mult n1 n2) = eval n1 * (eval n2)

solutionP :: [[Operator Parser Expr]] -> Parser Int
solutionP t = (sum <$> (fmap eval) <$> (many (exprP t))) <* eof

main = do
--  file <- openFile "day18.dat" ReadMode
  input <- hGetContents stdin
  parseTest (solutionP operatorTable1) input
  parseTest (solutionP operatorTable2) input
