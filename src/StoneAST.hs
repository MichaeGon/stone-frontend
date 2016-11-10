module StoneAST
    ( Primary(..)
    , Expr(..)
    , Stmt(..)
    , Type(..)
    , Env
    ) where

import qualified Data.Map as M

data Stmt = If Expr [Stmt] (Maybe [Stmt])
        | While Expr [Stmt]
        | Single Expr
        | Def String [(String, Type)] Type [Stmt]
        | Class String (Maybe String) [Stmt]
        | Var String Type Expr
        | Extern String [(String, Type)] Type
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

type Env = [M.Map String Type]

data Type = TInt
    | TString
    | TClassKey String
    | TFunction [Type] Type
    | TNative [Type] Type
    | TArray Type
    | TAny
    | TClassTree String [String] Env
    | Unknown
    deriving (Show, Eq)
