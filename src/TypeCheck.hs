{-# LANGUAGE TupleSections #-}
module TypeCheck where

import Control.Arrow ((&&&), (***), first)
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

pop' :: EnvState Env
pop' = fromJust <$> pop

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
typeCheckBlock xs = edit <$> mapM typeCheck xs
    where
        edit = fmap fst &&& (snd . last)

isSubTypeOf :: Type -> Type -> Bool
_ `isSubTypeOf` TAny = True

TFunction xs xt `isSubTypeOf` TFunction ys yt
                    = (length xs == length ys) && isSubTypeOf xt yt && all (uncurry isSubTypeOf) (zip xs ys)

TArray x `isSubTypeOf` TArray y
                    = x `isSubTypeOf` y

TClass xn xs `isSubTypeOf` TClass yn _
                    = xn == yn || elem yn xs

x `isSubTypeOf` y = x == y

union :: Type -> Type -> Type
union x y
    | x `isSubTypeOf` y = y
    | y `isSubTypeOf` x = x
    | otherwise = TAny

instance ITypeCheck Primary where
    typeCheck p@(Id s) = maybe' ("undefined identifier: " `mappend` s) (p,) <$> lookupEnv s

    typeCheck (DefApp prim xs) = check <$> typeCheck prim <*> mapM typeCheck xs
        where
            check = undefined

    typeCheck p@(Fun xs t b) = error "undefined defapp "
    typeCheck p@(Dot prim x) = error "undefined dot"
    typeCheck (Array xs) = edit <$> mapM typeCheck xs
        where
            edit [] = (Array xs, TArray Unknown)
            edit ys = (Array *** (TArray . last)) $ unzip ys
        --TArray . foldl' union Unknown <$> mapM typeCheck xs
    typeCheck (Index prim xs) = check <$> typeCheck prim <*> typeCheck xs
        where
            check (as, TArray at) (n, nt)
                | nt `isSubTypeOf` TInt = (Index as n, nt)
                | otherwise = error $ "expect Int at index but: " `mappend` show nt
            check (_, t) _ = error $ "expect array at index but: " `mappend` show t
        {-
        check <$> typeCheck prim <*> typeCheck xs
        where
            check (TArray arrt) nt
                | nt `isSubTypeOf` TInt = arrt
                | otherwise = error $ "expect Int at index but " `mappend` show nt
            check t _ = error $ "expect array at index but " `mappend` show t
            -}
    typeCheck p@(Num _) = return (p, TInt)
    typeCheck p@(Str _) = return (p, TString)


instance ITypeCheck Expr where
    typeCheck (Pos prim) = first Pos <$> typeCheck prim
    typeCheck (Neg prim) = check <$> typeCheck prim
        where
            check (n, nt)
                | nt `isSubTypeOf` TInt = (Neg n, nt)
                | otherwise = error $ "expect Int at neg but: " `mappend` show nt

    typeCheck (Bin l x r) = undefined

{-

instance ITypeCheck Stmt where
    typeCheck (If c b (Just e)) = check <$> typeCheck c <*> typeCheckBlock b <*> typeCheckBlock e
        where
            check ct bt et
                | ct `isSubTypeOf` TInt = ct `union` bt `union` et
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (If c b _) = check <$> typeCheck c <*> typeCheckBlock b
        where
            check ct bt
                | ct `isSubTypeOf` TInt = ct `union` bt
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (While c b) = check <$> typeCheck c <*> typeCheckBlock b
        where
            check ct bt
                | ct `isSubTypeOf` TInt = ct `union` bt
                | otherwise = error $ "expect Int at while condition but: " `mappend` show ct
    typeCheck (Def name xs t b) = lookupEnv name >>= maybe dv (const (error $ "duplicate definition: " `mappend` name))
        where
            dv = error "undefined def"

    typeCheck (Class name sc b) = lookupEnv name >>= maybe check (const (error $ "duplicate definition: " `mappend` name))
        where
            check = error "undefined class"

    typeCheck (Var name t e) = pop >>= check . fromJust
        where
            check z = maybe dv jf $ lookup name z
                where
                    dv = typeCheck e >>= check'
                    check' et
                        | t == Unknown = success et
                        | et `isSubTypeOf` t = success t
                        | otherwise = error $ "type mismatch at variable: " `mappend` name
                    jf = const (error $ "duplicate variable: " `mappend` name)
                    success x = x <$ push (insert name x z)

    typeCheck (Single e) = typeCheck e
-}

instance ITypeCheck Stmt where
    typeCheck (If c xs (Just e)) = check <$> typeCheck c <*> typeCheckBlock xs <*> typeCheckBlock e
        where
            check (cv, ct) (xvs, xt) (ev, et)
                | ct `isSubTypeOf` TInt = (If cv xvs (Just ev), ct `union` xt `union` et)
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (If c xs _) = check <$> typeCheck c <*> typeCheckBlock xs
        where
            check (cv, ct) (xvs, xt)
                | ct `isSubTypeOf` TInt =  (If cv xvs Nothing, ct `union` xt)
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (While c xs) = check <$> typeCheck c <*> typeCheckBlock xs
        where
            check (cv, ct) (xvs, xt)
                | ct `isSubTypeOf` TInt = (While cv xvs, ct `union` xt)
                | otherwise = error $ "expect Int at while condition but: " `mappend` show ct

    typeCheck (Def s xs t b) = evacEnv checkDup >> typeCheckArgs >>= undefined
        where
            checkDup = maybe () (error $ "duplicate definition: " `mappend` s) . lookup s <$> pop'
            typeCheckArgs = undefined
            
    typeCheck Class{} = undefined
    typeCheck (Var s t e) = evacEnv checkDup
                        >> typeCheck e >>= check
        where
            checkDup = maybe () (error $ "duplicate variable: " `mappend` s) . lookup s <$> pop'
            check (ev, et)
                | et `isSubTypeOf` t = (Var s t ev, t) <$ (pop' >>= push . insert s t)
                | otherwise = error "type mismatch at var"

    typeCheck (Single e) = first Single <$> typeCheck e
