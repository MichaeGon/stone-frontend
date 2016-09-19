module StoneFrontend
    ( Primary(..)
    , Factor(..)
    , Expr(..)
    , Stmt(..)
    , parseProgram
    , program
    ) where

import StoneLexer

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

parseProgram :: String -> Either ParseError [Stmt]
parseProgram = parse program ""

data Stmt = If Expr Stmt (Maybe Stmt) | While Expr Stmt | Block [Stmt] | Single Expr {-}[Expr]-} | Def String [String] Stmt-- | Null
    deriving (Show)
data Expr = Un Factor | Bin Expr String Expr
    deriving (Show)
data Factor = Neg Primary | Pos Primary
    deriving (Show)
data Primary = Paren Expr | Num Integer | Id String | Str String | DefApp Primary [Expr]
    deriving (Show)

program :: Parser [Stmt]
program = whiteSpace' *> program' <* eof
    where
        program' = many $ def <|> stmt

def :: Parser Stmt
def = reserved' "def" *> (Def <$> identifier' <*> parens' paramList <*> blockstmt)
    where
        paramList = identifier' `sepBy` try (char ',')

stmt :: Parser Stmt
stmt = choice
    [ blockstmt
    , single
    , ifstmt
    , whilestmt
    ] <?> "stmt"
    where
        single = Single <$> expr -- flip Single [] <$> expr
        ifstmt = reserved' "if" *> (If <$> expr <*> blockstmt <*> elseblock)
        elseblock = (reserved' "else" *> (Just <$> (ifstmt <|> blockstmt) ) ) <|> return Nothing
        whilestmt = reserved' "while" *> (While <$> expr <*> blockstmt)

blockstmt :: Parser Stmt
blockstmt = Block <$> braces' (many stmt)

expr :: Parser Expr
expr = l2s `chainr1` r1ops
    where
        l2s = l3s `chainl1` l2ops
        l3s = l4s `chainl1` l3ops
        l4s = unfact `chainl1` l4ops
        unfact = Un <$> factor
        binop x = (\l r -> Bin l x r) <$ reservedOp' x
        ops = choice . fmap binop
        r1ops = ops ["="]
        l2ops = ops ["==", "<", ">"]
        l3ops = ops ["+", "-"]
        l4ops = ops ["*", "/", "%"]

factor :: Parser Factor
factor = (reserved' "-" *> (Neg <$> primary)) <|> (Pos <$> primary)

primary :: Parser Primary
primary = choice primary' >>= check
    where
        primary' =
            [ Num <$> (try $ natural lexer)
            , Str <$> (try $ stringLiteral lexer)
            , Id <$> identifier'
            , Paren <$> parens' expr
            ]
        check p = (p <$ notFollowedBy (char '(')) <|> (DefApp p <$> parens' params)
        params = try expr `sepBy` try (char ',')

seps :: Parser Char
seps = oneOf "\n;"

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace lexer

reserved' :: String -> Parser ()
reserved' = try . reserved lexer

identifier' :: Parser String
identifier' = try $ identifier lexer

parens' :: Parser a -> Parser a
parens' = parens lexer

commaSep' :: Parser a -> Parser [a]
commaSep' = commaSep lexer

commaSep1' :: Parser a -> Parser [a]
commaSep1' = commaSep1 lexer

braces' :: Parser a -> Parser a
braces' = braces lexer

reservedOp' :: String -> Parser ()
reservedOp' = reservedOp lexer
