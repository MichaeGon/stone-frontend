{-# LANGUAGE TupleSections #-}
module TypeCheck where

import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import Data.List (foldl', foldl1')
import Data.Maybe
import Prelude hiding (lookup)
import qualified Data.Map as M

import StoneAST

type Env = [M.Map String Type]
type EnvState = State Env

class ITypeCheck a where
    typeCheck :: a -> EnvState (a, Type)

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

push :: Env -> EnvState ()
push = modify . mappend

lengthEnv :: EnvState Int
lengthEnv = length <$> get

splitEnvAt :: Int -> EnvState (Env, Env)
splitEnvAt n = splitAt n <$> get

maybe' :: String -> (a -> b) -> Maybe a -> b
maybe' = maybe . error

evacEnv :: EnvState a -> EnvState a
evacEnv x = get >>= (x >>=) . ($>) . put

typeCheckBlock :: (ITypeCheck a) => [a] -> EnvState ([a], Type)
typeCheckBlock [] = return ([], Unknown)
typeCheckBlock xs = (xs,) <$> foldl' (\acc x -> acc >> (snd <$> typeCheck x)) (return Unknown) xs

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
    typeCheck p@(Id s) = lookupEnv s >>= maybe' ("undefined identifier: " `mappend` s) (return . (p,))
    typeCheck p@(DefApp prim xs) = error "undefined defapp"
    typeCheck p@(Fun xs t b) = error "undefined defapp "
    typeCheck p@(Dot prim x) = error "undefined dot"
    typeCheck p@(Array xs) = (p,) . TArray . foldl1' union <$> mapM (fmap snd . typeCheck) xs
    typeCheck p@(Index prim xs) = check <$> typeCheck prim <*> typeCheck xs
        where
            check (Array _, TArray arrt) (_, nt)
                | nt `isSubTypeOf` TInt = (p, arrt)
                | otherwise = error $ "expect Int at index but " `mappend` show nt
            check (_, t) _ = error $ "expect array at index but " `mappend` show t
    typeCheck p@(Num _) = return (p, TInt)
    typeCheck p@(Str _) = return (p, TString)

instance ITypeCheck Expr where
    typeCheck e@(Pos p) = (e,) . snd <$> typeCheck p
    typeCheck e@(Neg p) = check <$> typeCheck p
        where
            check (_, pt)
                | pt `isSubTypeOf` TInt = (e, pt)
                | otherwise = error $ "expect Int at negative but: " `mappend` show pt
    typeCheck e@(Bin l x r) = error "undefined bin"

instance ITypeCheck Stmt where
    typeCheck s@(If c b (Just e)) = check <$> typeCheck c <*> typeCheckBlock b <*> typeCheckBlock e
        where
            check (_, ct) (_, bt) (_, et)
                | ct `isSubTypeOf` TInt = (s, ct `union` bt `union` et)
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck s@(If c b _) = check <$> typeCheck c <*> typeCheckBlock b
        where
            check (_, ct) (_, bt)
                | ct `isSubTypeOf` TInt = (s, ct `union` bt)
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck s@(While c b) = check <$> typeCheck c <*> typeCheckBlock b
        where
            check (_, ct) (_, bt)
                | ct `isSubTypeOf` TInt = (s, ct `union` bt)
                | otherwise = error $ "expect Int at while condition but: " `mappend` show ct
    typeCheck s@(Def name xs t b) = lookupEnv name >>= maybe check (const (error $ "duplicate definition: " `mappend` name))
        where
            check = error "undefined def"

    typeCheck s@(Class name sc b) = lookupEnv name >>= maybe check (const (error $ "duplicate definition: " `mappend` name))
        where
            check = error "undefined class"

    typeCheck s@(Var name t e) = pop >>= check . fromJust
        where
            check z = maybe dv jf $ lookup name z
                where
                    dv = typeCheck e >>= check'
                    check' (_, et)
                        | t == Unknown = success et
                        | et `isSubTypeOf` t = success t
                        | otherwise = error $ "type mismatch at variable: " `mappend` name
                    jf = const (error $ "duplicate variable: " `mappend` name)
                    success x = (s, x) <$ push (insert name x z)

    typeCheck s@(Single e) = (s,) . snd <$> typeCheck e
