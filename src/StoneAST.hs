module StoneAST
    ( Primary(..)
    , Expr(..)
    , Stmt(..)
    , Type(..)
    ) where

data Stmt = If Expr [Stmt] (Maybe [Stmt])
        | While Expr [Stmt]
        | Single Expr
        -- | Def String [String] [Stmt]
        | Def String [(String, Type)] Type [Stmt]
        | Class String (Maybe String) [Stmt]
        | Var String Type Expr
        -- | Var String Expr
    deriving (Show, Eq)

data Expr = Neg Primary | Pos Primary | Bin Expr String Expr
    deriving (Show, Eq)

data Primary = Paren Expr
        | Num Integer
        | Id String
        | Str String
        | DefApp Primary [Expr]
        | Fun [(String, Type)] Type [Stmt]
        | Dot Primary String
        | Array [Expr]
        | Index Primary Expr
    deriving (Show, Eq)

data Type = TInt
    | TString
    | TClass String
    | TFunction [Type] Type
    | TArray Type
    | TAny
    | Unknown
    deriving (Show, Eq)
