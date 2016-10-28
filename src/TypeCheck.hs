{-# LANGUAGE TupleSections #-}
module TypeCheck
    ( --runTypeCheck
    {-,-} ITypeCheck(..)
    --, EnvState
    , Parser
    , singleton
    ) where

import Control.Arrow ((&&&), (***), first)
import Control.Monad
import Control.Monad.State
import Data.Functor (($>))
import Data.List (foldl', foldl1')
import Data.Maybe
import Prelude hiding (lookup)
import Text.Parsec hiding (State, Parser)
import qualified Data.Map as M

import StoneAST

type Parser = Parsec String Env

--type EnvState = State Env

class ITypeCheck a where
    typeCheck :: a -> Parser (a, Type)--EnvState (a, Type)
    update :: a -> Type -> Parser a -- EnvState a

{-}
runTypeCheck :: [Stmt] -> [(Stmt, Type)]
runTypeCheck xs = undefined--runParser (mapM typeCheck xs) singleton
    --evalState (mapM typeCheck xs) singleton
-}

singleton :: Env
singleton = [M.empty]

insertEnv :: String -> Type -> Parser ()--EnvState ()
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

insert _ _ _ = {-unexpected "insert: empty type environment" --} error "insert: empty type environment"

lookupEnv :: String -> Parser (Maybe Type) -- EnvState (Maybe Type)
lookupEnv k = lookup k  <$> getState

lookup :: String -> Env -> Maybe Type
lookup k (x : xs) = maybe (lookup k xs) return $ M.lookup k x
lookup _ _ = Nothing

pop :: Parser (Maybe Env) --EnvState (Maybe Env)
pop = getState >>= modf
    where
        modf :: Env -> Parser (Maybe Env) --EnvState (Maybe Env)
        modf (x : xs) = return [x] <$ putState xs
        modf _ = return Nothing

pop' :: Parser Env -- EnvState Env
pop' = fromJust <$> pop

push :: Env -> Parser () -- EnvState ()
push = modifyState . mappend

lengthEnv :: Parser Int -- EnvState Int
lengthEnv = length <$> getState

splitEnvAt :: Int -> Parser (Env, Env) --EnvState (Env, Env)
splitEnvAt n = splitAt n <$> getState

maybe' :: String -> (a -> Parser b) -> Maybe a -> Parser b
maybe' = maybe . unexpected --maybe . error

evacEnv :: Parser a -> Parser a --EnvState a -> EnvState a
evacEnv x = getState >>= (x >>=) . ($>) . putState

typeCheckBlock :: (ITypeCheck a) => [a] -> Parser ([a], Type) --EnvState ([a], Type)
typeCheckBlock [] = return ([], Unknown)
typeCheckBlock xs = edit <$> mapM ff xs
    where
        ff x = typeCheck x >>= check
            where
                check (xv, xt)
                    | xt == Unknown = return (xv, xt)
                    | otherwise = (,xt) <$> update xv xt
        edit = fmap fst &&& (snd . last)


convertKey :: Type -> Parser Type -- EnvState Type
convertKey (TClassKey s) = fromMaybe (error $ "not found class: " `mappend` s) <$> lookupEnv s
convertKey t = return t

isSubTypeOf :: Type -> Type -> Bool
--Unknown `isSubTypeOf` _ = error "left unknown"
isSubTypeOf Unknown _ = error "isSubTypeOf: left unknown"

-- _ `isSubTypeOf` Unknown = error "right unknown"
isSubTypeOf _ Unknown = error "isSubTypeOf: right unknown"

{-}
TClassKey _ `isSubTypeOf` _ = error "left class key"
_ `isSubTypeOf` TClassKey _ = error "right class key"
-}

isSubTypeOf (TClassKey _) _ = error "isSubTypeOf: left class key"
isSubTypeOf _ (TClassKey _) = error "isSubTypeOf: right class key"

{-
TAny `isSubTypeOf` _ = True
_ `isSubTypeOf` TAny = True
-}
isSubTypeOf TAny _ = True
isSubTypeOf _ TAny = True

{-
TFunction xs xt `isSubTypeOf` TFunction ys yt
-}
isSubTypeOf (TFunction xs xt) (TFunction ys yt)
                    = (length xs == length ys) && isSubTypeOf xt yt && all (uncurry isSubTypeOf) (zip xs ys)

{-}
TArray x `isSubTypeOf` TArray y
-}
isSubTypeOf (TArray x) (TArray y)
                    = x `isSubTypeOf` y

{-
TClassTree xn xs _ `isSubTypeOf` TClassTree yn _ _
-}
isSubTypeOf (TClassTree xn xs _) (TClassTree yn _ _)
                    = yn `elem` (xn : xs)
                    -- = xn == yn || elem yn xs

{-}
x `isSubTypeOf` y = x == y
-}
isSubTypeOf x y = x == y

union :: Type -> Type -> Type
union x y
    | x == Unknown || y == Unknown = error "union: Unknown"
    | x `isSubTypeOf` y = y
    | y `isSubTypeOf` x = x
    | otherwise = TAny

instance ITypeCheck Primary where
    typeCheck (Paren e) = first Paren <$> typeCheck e

    typeCheck p@(Id s) = lookupEnv s >>= maybe' ("undefined identifier: " `mappend` s) (return . (p,))

    {-
    typeCheck (DefApp prim xs) = check <$> typeCheck prim <*> mapM typeCheck xs
        where
            check (p, TFunction ats rt) xvts
                | length ats == length xvts && checkArgs (zip ats xts) = (DefApp p xs', rt)
                | otherwise = unexpected {-error-} "type mismatch at function call"
                where
                    (xs', xts) = unzip xvts
            checkArgs = all (\(at, et) -> et == Unknown || et `isSubTypeOf` at)
    -}
    typeCheck (DefApp prim xs) = mapM typeCheck xs >>= checkFunc
        where
            checkFunc xvts = typeCheck prim >>= check xvts
            check xvts (p, TFunction ats rt)
                | length ats == length xvts && checkArgs (zip ats xts) = return (DefApp p xs', rt)
                | otherwise = unexpected "type mismatch at function call"
                where
                    (xs', xts) = unzip xvts
            checkArgs = all (\(at, et) -> et == Unknown || et `isSubTypeOf` at)

    {-
    typeCheck p@(Fun xs t' b) = initLocalEnv >>= push
                            >> typeCheckBlock b >>= build
        where
            initLocalEnv = foldl' (\acc (x, xt) -> insert x <$> convertKey xt <*> acc) (return singleton) xs

            build xts = build' xts <$> pop' <*> convertKey t'
            build' (bv, bt) z t = (\rt' ft' -> (Fun xs' rt' bv, ft')) <$> rt <*> ft
                where
                    xs' = zip (fst $ unzip xs) xts
                    ft = TFunction xts <$> rt
                    rt
                        | t == Unknown && t == bt = return TAny
                        | t == Unknown = return bt
                        | bt == Unknown = return t
                        | bt `isSubTypeOf` t = return t
                        | otherwise = unexpected {-error-} "type mismatch at closure"
                    xts = fmap ff xs
                    ff (x, _)
                        | rxt == Unknown = TAny
                        | otherwise = rxt
                        where
                            rxt = fromJust $ lookup x z
    -}
    typeCheck (Fun xs t b) = initLocalEnv >>= push
                        >> typeCheckBlock b >>= build
        where
            initLocalEnv = foldl' (\acc (x, xt) -> insert x <$> convertKey xt <*> acc) (return singleton) xs

            build xts = convertKey t >>= build'
                where
                    build' t' = pop' >>= checkFunc xts t'

            checkFunc (bv, bt) t' z = (\rt' ft' -> (Fun xs' rt' bv, ft')) <$> rt <*> ft
                where
                    xs' = zip (fst $ unzip xs) xts
                    ft = TFunction xts <$> rt
                    rt
                        | t' == Unknown && t' == bt = return TAny
                        | t' == Unknown = return bt
                        | bt == Unknown = return t'
                        | bt `isSubTypeOf` t' = return t'
                        | otherwise = unexpected "type mismatch at closure"
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
            getField = pop' >>= maybe' ("not found field: this." `mappend` x) (return . (p,)) . lookup x
    typeCheck (Dot p x) = typeCheck p >>= check
        where
            check (obj, TClassTree _ _ z) = maybe' ("not found field: " `mappend` x) (return . (Dot obj x,)) $ lookup x z

    typeCheck (Array xs) = edit <$> mapM typeCheck xs
        where
            edit [] = (Array xs, TArray Unknown)
            edit ys = (Array *** (TArray . unions . filter (/= Unknown))) $ unzip ys

            unions [] = Unknown
            unions ys = foldl1' union ys

    {-
    typeCheck (Index prim xs) = check <$> typeCheck prim <*> typeCheck xs
        where
            check (as, TArray at) (n, nt)
                | nt `isSubTypeOf` TInt = (Index as n, at)
                | otherwise = unexpected {-error-} $ "expect Int at index but: " `mappend` show nt
            check (_, t) _ = unexpected {-error-} $ "expect array at index but: " `mappend` show t
    -}
    typeCheck (Index prim xs) = typeCheck xs >>= checkArray
        where
            checkArray nvts = typeCheck prim >>= check nvts
            check (n, nt) (as, TArray at)
                | nt `isSubTypeOf` TInt = return (Index as n, at)
                | otherwise = unexpected $ "expect Int at index but: " `mappend` show nt
            check (_, t) _ = unexpected $ "expect array at index but: " `mappend` show t


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
                | otherwise = unexpected {-error-} $ "expect Int at neg but: " `mappend` show nt

    typeCheck (Bin l "=" r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (rv, rt) (lv, lt)
                | lt == Unknown && rt == lt = return (Bin lv "=" rv, lt)
                | lt == Unknown = (\lv' -> (Bin lv' "=" rv, rt)) <$> update lv rt
                | rt == Unknown = (\rv' -> (Bin lv "=" rv', lt)) <$> update rv lt
                | rt `isSubTypeOf` lt = return (Bin lv "=" rv, lt)
                | otherwise = unexpected {-error-} "type mismatch at assingexpr"

    typeCheck (Bin l "+" r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (lv, lt) (rv, rt)
                | lt == Unknown && lt == rt = (\lv' rv' -> (Bin lv' "+" rv', TInt)) <$> update lv TInt <*> update rv TInt
                | lt == Unknown = (\lv' -> (Bin lv' "+" rv, TInt `union` rt)) <$> update lv TInt
                | rt == Unknown = (\rv' -> (Bin lv "+" rv', lt `union` TInt)) <$> update rv TInt
                | any (isSubTypeOf lt) [TInt, TString] && any (isSubTypeOf rt) [TInt, TString] = return (Bin lv "+" rv, lt `union` rt)
                | otherwise = unexpected {-error-} "type mismatch at addexpr"

    typeCheck (Bin l "==" r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (rv, rt) (lv, lt)
                | lt == Unknown && lt == rt = return (Bin lv "==" rv, lt)
                | lt == Unknown = (\lv' -> (Bin lv' "==" rv, rt)) <$> update lv rt
                | rt == Unknown = (\rv' -> (Bin lv "==" rv', lt)) <$> update rv lt
                | rt `isSubTypeOf` lt || lt `isSubTypeOf` rt = return (Bin lv "==" rv, lt `union` rt)
                | otherwise = unexpected {-error-} "type mismatch at eqexpr"

    typeCheck (Bin l x r) = typeCheck r >>= checkLeft
        where
            checkLeft rvt = typeCheck l >>= check rvt
            check (rv, rt) (lv, lt)
                | lt == Unknown && lt == rt = (\lv' rv' -> (Bin lv' x rv', TInt)) <$> update lv TInt <*> update rv TInt
                | lt == Unknown = (\lv' -> (Bin lv' x rv, TInt)) <$> update lv TInt
                | rt == Unknown = (\rv' -> (Bin lv x rv', TInt)) <$> update rv TInt
                | rt `isSubTypeOf` TInt && lt `isSubTypeOf` TInt = return (Bin lv x rv, lt `union` rt)
                | otherwise = unexpected {-error-} $ "type mismatch at expr: " `mappend` x

    update (Pos p) t = Pos <$> update p t
    update (Neg p) t = Pos <$> update p t
    update p _ = return p

instance ITypeCheck Stmt where
    typeCheck (If c xs (Just e)) = typeCheck c >>= checkBody
        -- check <$> typeCheck c <*> typeCheckBlock xs <*> typeCheckBlock e
        where
            checkBody cvt = typeCheckBlock xs >>= checkElse cvt
            checkElse cvt xvts = typeCheckBlock e >>= check cvt xvts
            check (cv, ct) (xvs, xt) (ev, et)
                | xt == Unknown = unexpected {-error-} "unknown type at if body"
                | ct `isSubTypeOf` TInt = return (If cv xvs (Just ev), ct `union` xt `union` et)
                | otherwise = unexpected {-error-} $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (If c xs _) = typeCheck c >>= checkBody
        -- check <$> typeCheck c <*> typeCheckBlock xs
        where
            checkBody cvt = typeCheckBlock xs >>= check cvt
            check (cv, ct) (xvs, xt)
                | xt == Unknown = unexpected {-error-} "unknown type at if body"
                | ct `isSubTypeOf` TInt =  return (If cv xvs Nothing, ct `union` xt)
                | otherwise = unexpected {-error-} $ "expect Int at if condition but: " `mappend` show ct
    typeCheck (While c xs) = typeCheck c >>= checkBody
        -- check <$> typeCheck c <*> typeCheckBlock xs
        where
            checkBody cvt = typeCheckBlock xs >>= check cvt
            check (cv, ct) (xvs, xt)
                | xt == Unknown = unexpected {-error-} "unknown type at while body"
                | ct `isSubTypeOf` TInt = return (While cv xvs, ct `union` xt)
                | otherwise = unexpected {-error-} $ "expect Int at while condition but: " `mappend` show ct

    typeCheck (Def s xs t' b) = evacEnv checkDup
                            >> initLocalEnv >>= push
                            >> build
        where
            checkDup = pop' >>= maybe (return ()) (const (unexpected{-error-} $ "duplicate definition: " `mappend` s)) . lookup s--  <$> pop'

            initLocalEnv = foldl' (\acc (x, xt) -> insert x <$> convertKey xt <*> acc) (return singleton) xs

            build = typeCheckBlock b >>= build'
                where
                    build' rbs = pop' >>= check rbs
            check rbs z = convertKey t' >>= check' rbs z
            check' (bv, bt) z t = ft >>= insertFunc
                --(Def s xs' rt bv, ft) <$ insertEnv s ft
                where
                    insertFunc f = insertEnv s f
                                >> rt >>= return . (\rt' -> (Def s xs' rt' bv, f))
                    xs' = zip (fst $ unzip xs) rxs
                    ft = TFunction rxs <$> rt
                    rt
                        | t == Unknown && bt == t = return TAny
                        | t == Unknown = return bt
                        | bt == Unknown = return t
                        | bt `isSubTypeOf` t = return t
                        | otherwise = unexpected {-error-} "type mismatch at function"
                    rxs = fmap ff xs
                    ff (x, _)
                        | rxt == Unknown = TAny
                        | otherwise = rxt
                        where
                            rxt = fromJust $ lookup x z

    typeCheck (Class s sc xs) = maybe dv jf sc >>= build
        where
            dv = return ([], singleton)
            jf super = lookupEnv super >>= maybe' ("not found super class: " `mappend` super)  jf' -- <$> lookupEnv super
                where
                    jf' (TClassTree x ss z) = return (x : ss, z)
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
            checkDup = pop' >>= maybe (return ()) (const (unexpected {-error-} $ "duplicate variable: " `mappend` s)) . lookup s -- <$> pop'
            check evt = convertKey t' >>= check' evt
            check' (ev, et) t
                | t == Unknown && t == et = (Var s t ev, t) <$ (pop' >>= push . insert s t)
                | t == Unknown = (Var s et ev, et) <$ (pop' >>= push . insert s et)
                | et == Unknown = (Var s t ev, t) <$ (pop' >>= push . insert s t)
                | et `isSubTypeOf` t = (Var s t ev, t) <$ (pop' >>= push . insert s t)
                | otherwise = unexpected {-error-} "type mismatch at var"

    typeCheck (Single e) = first Single <$> typeCheck e

    update x _ = return x
