module StoneFrontend
    ( Primary(..)
    , Expr(..)
    , Stmt(..)
    , Type(..)
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

program :: Parser [Stmt]
program = whiteSpace' *> many program' <* eof

data Stmt = If Expr [Stmt] (Maybe [Stmt])
        | While Expr [Stmt]
        | Single Expr
        | Def String [(String, Type)] Type [Stmt]
        | Class String (Maybe String) [Stmt]
        | Var String Type Expr
    deriving (Show, Eq)

data Expr = Neg Primary | Pos Primary | Bin Expr String Expr
    deriving (Show, Eq)

data Primary = Paren Expr
        | Num Integer
        | Id String
        | Str String
        | DefApp Primary [Expr]
        | Fun [String] [Stmt]
        | Dot Primary String
        | Array [Expr]
        | Index Primary Expr
    deriving (Show, Eq)

data Type = TInt | TString | TAny | TClass String |Unknown
    deriving (Show, Eq)

program' :: Parser Stmt
program' = sep *> stmt' <* sep
    where
        stmt' = choice
            [ defclass
            , def
            , stmt
            ]

defclass :: Parser Stmt
defclass = reserved' "class" *> (Class <$> identifier' <*> superclass <*> classbody)
    where
        superclass = optionMaybe $ reserved' "extends" *> identifier'
        classbody = braces' $ many stmt'
        stmt' = sep *> (def <|> simple) <* sep

def :: Parser Stmt
def = reserved' "def" *> (Def <$> identifier' <*> paramList <*> typetag <*> block)
    where
        paramList = parens' . commaSep' $ param
        param = (,) <$> identifier' <*> typetag

stmt :: Parser Stmt
stmt = choice
    [ variable
    , ifstmt
    , whilestmt
    , simple
    ]
    where
        ifstmt = reserved' "if" *> (If <$> expr <*> block <*> optionMaybe elsestmt)
        elsestmt = reserved' "else" *> (((:[]) <$> ifstmt) <|> block)
        whilestmt = reserved' "while" *> (While <$> expr <*> block)
        variable = reserved' "var" *> (Var <$> identifier' <*> typetag <*> (reservedOp' "=" *> expr))

simple :: Parser Stmt
simple = Single <$> expr

block :: Parser [Stmt]
block = braces' $ many stmt'
    where
        stmt' = sep *> stmt <* sep

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
primary = closure <|> primary'
    where
        primary' = choice
            [ Num <$> try (natural lexer)
            , Id <$> identifier'
            , Str <$> try (stringLiteral lexer)
            , Paren <$> parens' expr
            , Array <$> brackets' (commaSep' expr)
            ] >>= check

        check p = option p $ postfix >>= check
            where
                postfix = (reservedOp' "." *> (Dot p <$> identifier'))
                    <|> (notFollowedBy (char '(') *> (Index p <$> brackets' expr))
                    <|> (DefApp p <$> parens' (commaSep' expr))

        closure = reserved' "fun" *> (Fun <$> params <*> block)
        params = parens' $ commaSep' identifier'

typetag :: Parser Type
typetag = option Unknown $ reservedOp' ":" *> choice tags
    where
        tags =
            [ TInt <$ reserved' "Int"
            , TString <$ reserved' "String"
            , TAny <$ reserved' "Any"
            , TClass <$> identifier'
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

brackets' :: Parser a -> Parser a
brackets' = brackets lexer

commaSep' :: Parser a -> Parser [a]
commaSep' x = x `sepBy` (whiteSpace' *> char ',' <* whiteSpace')
