{-# LANGUAGE TupleSections #-}
module TypeCheck where

import Control.Monad
--import Control.Monad.State
import Data.List (foldl')
import Prelude hiding (lookup)
import Text.Parsec hiding (Parser)
import qualified Data.Map as M

import StoneAST

type Parser = Parsec String (Type, Env)

type Env = [M.Map String Type]
--type EnvState = State (Type, Env)


data Type = TInt | TString | TAny | TClass String |Unknown
    deriving (Show, Eq)

class ITypeCheck a where
    typeCheck :: a -> Parser a

singleton :: Env
singleton = [M.empty]

insertEnv :: String -> Type -> Parser ()
insertEnv k v = getState >>= putState . (v,) . insert k v . snd

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
lookupEnv k = lookup k . snd <$> getState

lookup :: String -> Env -> Maybe Type
lookup k (x : xs) = maybe (lookup k xs) return $ M.lookup k x
lookup _ _ = Nothing

pop :: Parser (Maybe Env)
pop = getState >>= modf
    where
        modf :: (Type, Env) -> Parser (Maybe Env)
        modf (t, x : xs) = return [x] <$ putState (t, xs)
        modf _ = return Nothing

lengthEnv :: Parser Int
lengthEnv = length . snd <$> getState

splitEnvAt :: Int -> Parser (Env, Env)
splitEnvAt n = splitAt n . snd <$> getState

maybe' :: String -> (a -> Parser b) -> Maybe a -> Parser b
maybe' = maybe . fail

modifyType :: (ITypeCheck a) => a -> Type -> Parser a
modifyType p t = p <$ modifyState ((t,) . snd)

instance ITypeCheck Expr where
    typeCheck _ = undefined

instance ITypeCheck Primary where
    typeCheck p@(Paren e) = p <$ typeCheck e
    typeCheck p@(Num _) = modifyType p TInt
    typeCheck p@(Str _) = modifyType p TString

    typeCheck p@(Id s) = lookupEnv s >>= maybe' ("not found: " `mappend` s) (modifyType p)

    typeCheck _ = undefined
