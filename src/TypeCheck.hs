{-# LANGUAGE TupleSections #-}
module TypeCheck
    ( runTypeCheck
    , ITypeCheck(..)
    , EnvState
    ) where

import Control.Arrow ((&&&), (***), first)
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import Data.List (foldl', foldl1')
import Data.Maybe
import Prelude hiding (lookup)
import qualified Data.Map as M

import StoneAST

type EnvState = State Env

class ITypeCheck a where
    typeCheck :: a -> EnvState (a, Type)
    update :: a -> Type -> EnvState a

runTypeCheck :: [Stmt] -> [(Stmt, Type)]
runTypeCheck xs = evalState (mapM typeCheck xs) singleton

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
typeCheckBlock xs = edit <$> mapM ff xs
    where
        ff x = typeCheck x >>= check
            where
                check (xv, xt)
                    | xt == Unknown = return (xv, xt)
                    | otherwise = (,xt) <$> update xv xt
        edit = fmap fst &&& (snd . last)


convertKey :: Type -> EnvState Type
convertKey (TClassKey s) = fromMaybe (error $ "not found class: " `mappend` s) <$> lookupEnv s
convertKey t = return t

isSubTypeOf :: Type -> Type -> Bool
Unknown `isSubTypeOf` _ = error "left unknown"
_ `isSubTypeOf` Unknown = error "right unknown"

TClassKey _ `isSubTypeOf` _ = error "left class key"
_ `isSubTypeOf` TClassKey _ = error "right class key"

TAny `isSubTypeOf` _ = True
_ `isSubTypeOf` TAny = True

TFunction xs xt `isSubTypeOf` TFunction ys yt
                    = (length xs == length ys) && isSubTypeOf xt yt && all (uncurry isSubTypeOf) (zip xs ys)

TArray x `isSubTypeOf` TArray y
                    = x `isSubTypeOf` y

TClassTree xn xs _ `isSubTypeOf` TClassTree yn _ _
                    = xn == yn || elem yn xs

x `isSubTypeOf` y = x == y

union :: Type -> Type -> Type
union x y
    | x == Unknown || y == Unknown = error "union: Unknown"
    | x `isSubTypeOf` y = y
    | y `isSubTypeOf` x = x
    | otherwise = TAny

instance ITypeCheck Primary where
    typeCheck (Paren e) = first Paren <$> typeCheck e

    typeCheck p@(Id s) = maybe' ("undefined identifier: " `mappend` s) (p,) <$> lookupEnv s

    typeCheck (DefApp prim xs) = check <$> typeCheck prim <*> mapM typeCheck xs
        where
            check (p, TFunction ats rt) xvts
                | length ats == length xvts && checkArgs (zip ats xts) = (DefApp p xs', rt)
                | otherwise = error "type mismatch at function call"
                where
                    (xs', xts) = unzip xvts
            checkArgs = all (\(at, et) -> et == Unknown || et `isSubTypeOf` at)

    typeCheck p@(Fun xs t' b) = initLocalEnv >>= push
                            >> typeCheckBlock b >>= build
        where
            initLocalEnv = foldl' (\acc (x, xt) -> insert x <$> convertKey xt <*> acc) (return singleton) xs

            build xts = build' xts <$> pop' <*> convertKey t'
            build' (bv, bt) z t = (Fun xs' rt bv, ft)
                where
                    xs' = zip (fst $ unzip xs) xts
                    ft = TFunction xts rt
                    rt
                        | t == Unknown && t == bt = TAny
                        | t == Unknown = bt
                        | bt == Unknown = t
                        | bt `isSubTypeOf` t = t
                        | otherwise = error "type mismatch at closure"
                    xts = fmap ff xs
                    ff (x, _)
                        | rxt == Unknown = TAny
                        | otherwise = rxt
                        where
                            rxt = fromJust $ lookup x z

    typeCheck (Dot p "new") = check <$> typeCheck p
        where
            check (cv, ct) = (Dot cv "new", ct)

    typeCheck p@(Dot (Id "this") x) = evacEnv getField
        where
            getField = maybe' ("not found field: this." `mappend` x) (p,) . lookup x <$> pop'

    typeCheck (Dot p x) = check <$> typeCheck p
        where
            check (obj, TClassTree _ _ z) = maybe' ("not found field: " `mappend` x) (Dot obj x,) $ lookup x z

    typeCheck (Array xs) = edit <$> mapM typeCheck xs
        where
            edit [] = (Array xs, TArray Unknown)
            edit ys = (Array *** (TArray . unions . filter (/= Unknown))) $ unzip ys

            unions [] = Unknown
            unions ys = foldl1' union ys

    typeCheck (Index prim xs) = check <$> typeCheck prim <*> typeCheck xs
        where
            check (as, TArray at) (n, nt)
                | nt `isSubTypeOf` TInt = (Index as n, at)
                | otherwise = error $ "expect Int at index but: " `mappend` show nt
            check (_, t) _ = error $ "expect array at index but: " `mappend` show t

    typeCheck p@(Num _) = return (p, TInt)
    typeCheck p@(Str _) = return (p, TString)

    update p@(Id s) t = p <$ insertEnv s t
    update (Paren e) t = Paren <$> update e t
    update (Index p n) t = flip Index n <$> update p (TArray t)
    update p@(Dot (Id "this") x) t = p <$ (pop' >>= push . insert x t)
    update (Dot p x) t = typeCheck p >>= update'
        where
            update' (obj, TClassTree s ss z) = Dot obj x <$ (insertEnv s . TClassTree s ss $ insert x t z )
    update p _ = return p


instance ITypeCheck Expr where
    typeCheck (Pos prim) = first Pos <$> typeCheck prim
    typeCheck (Neg prim) = typeCheck prim >>= check
        where
            check (n, nt)
                | nt == Unknown = (Neg n, TInt) <$ update n TInt
                | nt `isSubTypeOf` TInt = return (Neg n, nt)
                | otherwise = error $ "expect Int at neg but: " `mappend` show nt

    typeCheck (Bin l "=" r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (rv, rt) (lv, lt)
                | lt == Unknown && rt == lt = return (Bin lv "=" rv, lt)
                | lt == Unknown = (\lv' -> (Bin lv' "=" rv, rt)) <$> update lv rt
                | rt == Unknown = (\rv' -> (Bin lv "=" rv', lt)) <$> update rv lt
                | rt `isSubTypeOf` lt = return (Bin lv "=" rv, lt)
                | otherwise = error "type mismatch at assingexpr"

    typeCheck (Bin l "+" r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (lv, lt) (rv, rt)
                | lt == Unknown && lt == rt = (\lv' rv' -> (Bin lv' "+" rv', TInt)) <$> update lv TInt <*> update rv TInt
                | lt == Unknown = (\lv' -> (Bin lv' "+" rv, TInt `union` rt)) <$> update lv TInt
                | rt == Unknown = (\rv' -> (Bin lv "+" rv', lt `union` TInt)) <$> update rv TInt
                | any (isSubTypeOf lt) [TInt, TString] && any (isSubTypeOf rt) [TInt, TString] = return (Bin lv "+" rv, lt `union` rt)
                | otherwise = error "type mismatch at addexpr"

    typeCheck (Bin l "==" r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (rv, rt) (lv, lt)
                | lt == Unknown && lt == rt = return (Bin lv "==" rv, lt)
                | lt == Unknown = (\lv' -> (Bin lv' "==" rv, rt)) <$> update lv rt
                | rt == Unknown = (\rv' -> (Bin lv "==" rv', lt)) <$> update rv lt
                | rt `isSubTypeOf` lt || lt `isSubTypeOf` rt = return (Bin lv "==" rv, lt `union` rt)
                | otherwise = error "type mismatch at eqexpr"

    typeCheck (Bin l x r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (rv, rt) (lv, lt)
                | lt == Unknown && lt == rt = (\lv' rv' -> (Bin lv' x rv', TInt)) <$> update lv TInt <*> update rv TInt
                | lt == Unknown = (\lv' -> (Bin lv' x rv, TInt)) <$> update lv TInt
                | rt == Unknown = (\rv' -> (Bin lv x rv', TInt)) <$> update rv TInt
                | rt `isSubTypeOf` TInt && lt `isSubTypeOf` TInt = return (Bin lv x rv, lt `union` rt)
                | otherwise = error $ "type mismatch at expr: " `mappend` x

    update (Pos p) t = Pos <$> update p t
    update (Neg p) t = Pos <$> update p t
    update p _ = return p

instance ITypeCheck Stmt where
    typeCheck (If c xs (Just e)) = check <$> typeCheck c <*> typeCheckBlock xs <*> typeCheckBlock e
        where
            check (cv, ct) (xvs, xt) (ev, et)
                | xt == Unknown = error "unknown type at if body"
                | ct `isSubTypeOf` TInt = (If cv xvs (Just ev), ct `union` xt `union` et)
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (If c xs _) = check <$> typeCheck c <*> typeCheckBlock xs
        where
            check (cv, ct) (xvs, xt)
                | xt == Unknown = error "unknown type at if body"
                | ct `isSubTypeOf` TInt =  (If cv xvs Nothing, ct `union` xt)
                | otherwise = error $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (While c xs) = check <$> typeCheck c <*> typeCheckBlock xs
        where
            check (cv, ct) (xvs, xt)
                | xt == Unknown = error "unknown type at while body"
                | ct `isSubTypeOf` TInt = (While cv xvs, ct `union` xt)
                | otherwise = error $ "expect Int at while condition but: " `mappend` show ct

    typeCheck (Def s xs t' b) = evacEnv checkDup
                            >> initLocalEnv >>= push
                            >> build
        where
            checkDup = maybe () (error $ "duplicate definition: " `mappend` s) . lookup s <$> pop'

            initLocalEnv = foldl' (\acc (x, xt) -> insert x <$> convertKey xt <*> acc) (return singleton) xs

            build = typeCheckBlock b >>= build'
                where
                    build' rbs = pop' >>= check rbs
            check rbs z = convertKey t' >>= check' rbs z
            check' (bv, bt) z t = (Def s xs' rt bv, ft) <$ insertEnv s ft
                where
                    xs' = zip (fst $ unzip xs) rxs
                    ft = TFunction rxs rt
                    rt
                        | t == Unknown && bt == t = TAny
                        | t == Unknown = bt
                        | bt == Unknown = t
                        | bt `isSubTypeOf` t = t
                        | otherwise = error "type mismatch at function"
                    rxs = fmap ff xs
                    ff (x, _)
                        | rxt == Unknown = TAny
                        | otherwise = rxt
                        where
                            rxt = fromJust $ lookup x z

    typeCheck (Class s sc xs) = maybe dv jf sc >>= build
        where
            dv = return ([], singleton)
            jf super = maybe' ("not found super class: " `mappend` super)  jf' <$> lookupEnv super
                where
                    jf' (TClassTree x ss z) = (x : ss, z)
            build (ss, z) = push z
                        >> typeCheckBlock xs >>= checkEnv . fst
                where
                    checkEnv xs' = pop' >>= build' xs'
                    build' xs' z' = (Class s sc xs', ct) <$ insertEnv s ct
                        where
                            ct = TClassTree s ss z'

    typeCheck (Var s t' e) = evacEnv checkDup
                        >> typeCheck e >>= check
        where
            checkDup = maybe () (error $ "duplicate variable: " `mappend` s) . lookup s <$> pop'
            check evt = convertKey t' >>= check' evt
            check' (ev, et) t
                | t == Unknown && t == et = (Var s t ev, t) <$ (pop' >>= push . insert s t)
                | t == Unknown = (Var s et ev, et) <$ (pop' >>= push . insert s et)
                | et == Unknown = (Var s t ev, t) <$ (pop' >>= push . insert s t)
                | et `isSubTypeOf` t = (Var s t ev, t) <$ (pop' >>= push . insert s t)
                | otherwise = error "type mismatch at var"

    typeCheck (Single e) = first Single <$> typeCheck e

    update x _ = return x
