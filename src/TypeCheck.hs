{-# LANGUAGE TupleSections #-}
module TypeCheck where

import Control.Monad
import Control.Monad.State
import Data.List (foldl')
import Prelude hiding (lookup)
import qualified Data.Map as M

import StoneAST

type Env = [M.Map String Type]
type EnvState = State Env

class ITypeCheck a where
    typeCheck :: a -> EnvState a

singleton :: Env
singleton = [M.empty]

insertEnv :: String -> Type -> EnvState ()
insertEnv k v = get >>= put . insert k v

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

lookupEnv :: String -> EnvState (Maybe Type)
lookupEnv k = lookup k  <$> get

lookup :: String -> Env -> Maybe Type
lookup k (x : xs) = maybe (lookup k xs) return $ M.lookup k x
lookup _ _ = Nothing

pop :: EnvState (Maybe Env)
pop = get >>= modf
    where
        modf :: Env -> EnvState (Maybe Env)
        modf (x : xs) = return [x] <$ put xs
        modf _ = return Nothing

lengthEnv :: EnvState Int
lengthEnv = length <$> get

splitEnvAt :: Int -> EnvState (Env, Env)
splitEnvAt n = splitAt n <$> get

maybe' :: String -> (a -> b) -> Maybe a -> b
maybe' = maybe . error

isSubTypeOf :: Type -> Type -> Bool
_ `isSubTypeOf` TAny = True

TFunction xs xt `isSubTypeOf` TFunction ys yt = (length xs == length ys)
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

instance ITypeCheck Primary where
    typeCheck p@(Id s) = error "undefined id"
    typeCheck p@(DefApp prim xs) = error "undefined defapp"
    typeCheck p@(Fun xs t b) = error "undefined defapp "
    typeCheck p@(Dot prim x) = error "undefined dot"
    typeCheck p@(Array xs) = error "undefined array"
    typeCheck p@(Index prim xs) = error "undefined index"
    typeCheck p = return p

instance ITypeCheck Expr where
    typeCheck e@(Pos p) = e <$ typeCheck p
    typeCheck e@(Neg p) = error "undefined neg"
    typeCheck e@(Bin l x r) = error "undefined bin"

instance ITypeCheck Stmt where
    typeCheck s@(If c b e) = error "undefined if"
    typeCheck s@(While c b) = error "undefined if"
    typeCheck s@(Def name xs t b) = error "undefined def"
    typeCheck s@(Class name sc b) = error "undefined class"
    typeCheck s@(Var name t e) = error "undefined var"
    typeCheck s@(Single e) = s <$ typeCheck e
