module StoneFrontend
    ( Primary(..)
    --, Factor(..)
    , Expr(..)
    , Stmt(..)
    , parseProgram
    , program
    ) where

import StoneLexer

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token

parseProgram :: String -> Either ParseError [Stmt]
parseProgram = parse program ""

--parseProgram = parse program' ""

program :: Parser [Stmt]
program = whiteSpace' *> many program' <* eof

data Stmt = If Expr [Stmt] (Maybe [Stmt])
        | While Expr [Stmt]
        | Single Expr
    deriving (Show)

data Expr = Neg Primary | Pos Primary | Bin Expr String Expr
    deriving (Show)

data Primary = Paren Expr | Num Integer | Id String | Str String
    deriving (Show)

program' :: Parser Stmt
program' = sep *> stmt' <* sep
    where
        stmt' = stmt

stmt :: Parser Stmt
stmt = choice
    [ ifstmt
    , whilestmt
    , simple
    ]
    where
        ifstmt = reserved' "if" *> (If <$> expr <*> block <*> optionMaybe elsestmt)
        elsestmt = reserved' "else" *> (((:[]) <$> ifstmt) <|> block)
        whilestmt = reserved' "while" *> (While <$> expr <*> block)

simple :: Parser Stmt
simple = Single <$> expr

block :: Parser [Stmt]
block = braces' $ many stmt'
    where
        stmt' = stmt <* sep

expr :: Parser Expr
expr = chainr1 l2s r1ops
    where
        l2s = chainl1 l3s l2ops
        l3s = chainl1 l4s l3ops
        l4s = chainl1 factor l4ops

        factor = (reservedOp' "-" *> (Neg <$> primary))
            <|> (Pos <$> primary)

        binop x = (\l r -> Bin l x r) <$ reservedOp' x
        ops = choice . fmap binop

        r1ops = ops ["="]
        l2ops = ops ["==", "<", ">"]
        l3ops = ops ["+", "-"]
        l4ops = ops ["*", "/", "%"]

primary :: Parser Primary
primary = choice
    [ Num <$> try (natural lexer)
    , Id <$> identifier'
    , Str <$> try (stringLiteral lexer)
    , Paren <$> parens' expr
    ]

sep :: Parser ()
sep = void . many $ reservedOp' ";"

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace lexer

reserved' :: String -> Parser ()
reserved' = reserved lexer

reservedOp' :: String -> Parser ()
reservedOp' = reservedOp lexer

identifier' :: Parser String
identifier' = identifier lexer

parens' :: Parser a -> Parser a
parens' = parens lexer

braces' :: Parser a -> Parser a
braces' = braces lexer
{-
data Stmt = If Expr Stmt (Maybe Stmt) | While Expr Stmt | Block [Stmt] | Single Expr | Def String [String] Stmt | Class String (Maybe String) [Stmt]
    deriving (Show)
data Expr = Un Factor | Bin Expr String Expr
    deriving (Show)
data Factor = Neg Primary | Pos Primary
    deriving (Show)
data Primary = Paren Expr | Num Integer | Id String | Str String | DefApp Primary [Expr] | Fun [String] Stmt | Dot Primary String
    deriving (Show)

program :: Parser [Stmt]
program = whiteSpace' *> program' <* eof
    where
        program' = many . choice $ -- $ def <|> stmt
            [ defclass
            , def
            , stmt
            ]

defclass :: Parser Stmt
defclass = reserved' "class" *> (Class <$> identifier' <*> superclass <*> classbody)
    where
        superclass = (reserved' "extends" *> (Just <$> identifier')) <|> return Nothing
        classbody = braces' . many $ def <|> single

def :: Parser Stmt
def = reserved' "def" *> (Def <$> identifier' <*> parens' paramList <*> blockstmt)
    where
        paramList = identifier' `sepBy` try (whiteSpace' *> char ',' <* whiteSpace')

stmt :: Parser Stmt
stmt = choice
    [ blockstmt
    , single
    , ifstmt
    , whilestmt
    ] <?> "stmt"
    where
        --single = Single <$> expr
        ifstmt = reserved' "if" *> (If <$> expr <*> blockstmt <*> elseblock)
        elseblock = (reserved' "else" *> (Just <$> (ifstmt <|> blockstmt) ) ) <|> return Nothing
        whilestmt = reserved' "while" *> (While <$> expr <*> blockstmt)

single :: Parser Stmt
single = Single <$> expr

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
factor = (reservedOp' "-" *> (Neg <$> primary)) <|> (Pos <$> primary)

primary :: Parser Primary
primary = closure <|> (choice primary' >>= check)
    where
        primary' =
            [ Num <$> (try $ natural lexer)
            , Str <$> (try $ stringLiteral lexer)
            , Id <$> identifier'
            , Paren <$> parens' expr
            ]
        check p = (reservedOp' "." *> (identifier' >>= check . Dot p)) <|> (p <$ notFollowedBy (char '(')) <|> (parens' params >>= check . DefApp p)
        params = try expr `sepBy` try (whiteSpace' *> char ',' <* whiteSpace')
        closure = reserved' "fun" *> (Fun <$> parens' paramList <*> blockstmt)
        paramList = identifier' `sepBy` try (whiteSpace' *> char ',' <* whiteSpace')

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
-}
