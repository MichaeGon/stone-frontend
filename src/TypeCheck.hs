{-# LANGUAGE TupleSections #-}
module TypeCheck where

import Control.Monad
--import Control.Monad.State
import Data.List (foldl')
import Prelude hiding (lookup)
import Text.Parsec hiding (Parser)
import qualified Data.Map as M

import StoneAST

type Parser = Parsec String Env

type Env = [M.Map String Type]
--type EnvState = State (Type, Env)


data Type = TInt
    | TString
    | TClass String [String]
    | TFunction [Type] Type Env
    | TArray Type
    | TAny
    | Unknown
    deriving (Show, Eq)

class ITypeCheck a where
    typeCheck :: a -> Parser (a, Type)

singleton :: Env
singleton = [M.empty]

insertEnv :: String -> Type -> Parser ()
insertEnv k v = getState >>= putState . insert k v

insert :: String -> Type -> Env -> Env
insert k v xxs@(x : xs)
    | all (M.notMember k) xxs = M.insert k v x : xs
    | otherwise = recinsert xxs
    where
        recinsert (y : ys)
            | M.member k y = M.insert k v y : ys
            | otherwise = y : recinsert ys
        recinsert _ = []

insert _ _ _ = error "insert: empty type environment"

lookupEnv :: String -> Parser (Maybe Type)
lookupEnv k = lookup k  <$> getState

lookup :: String -> Env -> Maybe Type
lookup k (x : xs) = maybe (lookup k xs) return $ M.lookup k x
lookup _ _ = Nothing

pop :: Parser (Maybe Env)
pop = getState >>= modf
    where
        modf :: Env -> Parser (Maybe Env)
        modf (x : xs) = return [x] <$ putState xs
        modf _ = return Nothing

lengthEnv :: Parser Int
lengthEnv = length <$> getState

splitEnvAt :: Int -> Parser (Env, Env)
splitEnvAt n = splitAt n <$> getState

maybe' :: String -> (a -> Parser b) -> Maybe a -> Parser b
maybe' = maybe . fail

isSubTypeOf :: Type -> Type -> Bool
_ `isSubTypeOf` TAny = True

TFunction xs xt _ `isSubTypeOf` TFunction ys yt _ = (length xs == length ys)
                                                && isSubTypeOf xt yt
                                                && all (uncurry isSubTypeOf) (zip xs ys)

TArray x `isSubTypeOf` TArray y = x `isSubTypeOf` y

TClass xn xs `isSubTypeOf` TClass yn _ = xn == yn || elem yn xs

x `isSubTypeOf` y = x == y

union :: Type -> Type -> Type
union x y
    | x `isSubTypeOf` y = y
    | y `isSubTypeOf` x = x
    | otherwise = TAny

instance ITypeCheck Stmt where
    typeCheck p@(If c b e) = fail "undefined"
    typeCheck _ = fail "undefined stmt"

instance ITypeCheck Expr where
    typeCheck _ = fail "undefined expr"

instance ITypeCheck Primary where
    typeCheck _ = fail "undefined primary"
