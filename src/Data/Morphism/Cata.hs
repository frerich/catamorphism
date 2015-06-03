{-|
Module:         Data.Morphism.Cata
Copyright:      (c) 2014 Frerich Raabe
License:        BSD3
Maintainer:     frerich.raabe@gmail.com
Stability:      experimental

This module exposes a 'makeCata' function which can create catamorphisms for
arbitrary Haskell types. Catamorphisms are functions which deconstruct some
value by replacing each data constructor with a custom function yielding a new
value. See <http://www.haskell.org/haskellwiki/Catamorphisms> for a more
in-depth discussion of catamorphisms in Haskell.

The Haskell base package already comes with a couple of standard catamorphisms
such as 'bool' (for Bool values), 'maybe' (for Maybe values) values, 'either'
for (Either values) values and 'foldr' (for lists). These catamorphisms could
have been generated using 'makeCata' as follows:

> -- Defines 'bool :: a -> a -> Bool -> a'
> $(makeCata defaultOptions ''Bool)
>
> -- Defines 'maybe :: b -> (a -> b) -> Maybe a -> b'
> $(makeCata defaultOptions ''Maybe)
>
> -- Defines 'either :: (a -> c) -> (b -> c) -> Either a b -> c'
> $(makeCata defaultOptions ''Either)

However, catamorphisms are especially useful for recursive data structures. Consider
the following simple example which defines a basic data type for modelling sums
of numbers, supporting variables:

> {-# LANGUAGE TemplateHaskell #-}
>
> import Data.Morphism.Cata
> import Data.Maybe (fromJust)
>
> data Expr a = Number a
>             | Variable Char
>             | Sum (Expr a) (Expr a)
>
> -- Defines 'cataExpr :: (a -> b) -> (Char -> b) -> (b -> b -> b) -> Expr a -> b'
> $(makeCata defaultOptions { cataName = "cataExpr" } ''Expr)

The 'makeCata' invocation defines a 'cataExpr' function which works like a fold on
'Expr' values; it can be used to implement various useful other functions:

> -- Evaluate an Expr, given some variable bindings
> eval :: Num a => [(Char, a)] -> Expr a -> a
> eval vars = cataExpr id (fromJust . (`lookup` vars)) (+)
>
> -- Pretty-prints an Expr
> pprint :: Show a => Expr a -> String
> pprint = cataExpr show show (\a b -> a ++ " + " ++ b)
>
> -- Counts the number of variables used in an expr
> numVars :: Expr a -> Int
> numVars = cataExpr (const 1) (const 0) (+)
-}

module Data.Morphism.Cata
    ( CataOptions(..)
    , defaultOptions
    , makeCata
    )
where

import Control.Monad (forM, replicateM)

import Data.Char (toLower)
import Data.Functor ((<$>))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (mkNameG, NameSpace(TcClsName))

{-|
    Values of the 'CataOptions' type can be passed to 'makeCata' in order to
    customize the generated catamorphism. At this point, only the name of the
    function can be changed.
-}
data CataOptions = CataOptions {
    {-|
        The desired name for the catamorphism. An empty string will make
        'makeCata' derive the catamorphism name from the type by just taking
        the type name and making the first letter lower-case.
    -}
    cataName :: String
}

{-|
    The default catamorphism generation options; the catamorphism will be named
    after the type, e.g.

    > $(makeCata defaultOptions ''Bool)

    defines a function 'bool'.
-}
defaultOptions :: CataOptions
defaultOptions = CataOptions ""

listName :: Name
listName = mkNameG TcClsName "ghc-prim" "GHC.Types" "[]"

makeFuncT :: Type -> Type -> Type
makeFuncT a = AppT (AppT ArrowT a)

conArgTypes :: Con -> [Type]
conArgTypes (NormalC _ args)     = map snd args
conArgTypes (RecC _ args)        = map (\(_,_,x) -> x) args
conArgTypes (InfixC arg1 _ arg2) = map snd [arg1, arg2]
conArgTypes (ForallC _ _ c)      = conArgTypes c

conName :: Con -> Name
conName (NormalC n _)   = n
conName (RecC n _)      = n
conName (InfixC _ n _)  = n
conName (ForallC _ _ c) = conName c

typeName :: Type -> Maybe Name
typeName (AppT t _) = typeName t
typeName (ConT n)   = Just n
typeName ListT      = Just listName
typeName _          = Nothing

conType :: Name -> Name -> Con -> Type
conType inputT resultT c = foldr makeFuncT (VarT resultT) argTypes
  where
    argTypes = map fixupArgType (conArgTypes c)

    fixupArgType t = case typeName t of
                        Just n  -> if n == inputT then VarT resultT else t
                        Nothing -> t

-- |The 'makeCata' function creates a catamorphism for the given type.
makeCata :: CataOptions     -- Options to customize the catamorphism; the name of the defined function can be changed
         -> Name            -- The type to generate a catamorphism for.
         -> Q [Dec]
makeCata opts ty = do
    typeInfo <- reify ty
    (tyVarBndrs, cons) <- case typeInfo of
            TyConI (DataD _ _ tyVarBndrs cons _)   -> return (tyVarBndrs, cons)
            TyConI (NewtypeD _ _ tyVarBndrs con _) -> return (tyVarBndrs, [con])
            _                                      -> fail "makeCata: Expected name of type constructor"
    sequence [signature tyVarBndrs cons, funDef cons]
  where
    signature :: [TyVarBndr] -> [Con] -> Q Dec
    signature tyVarBndrs cons = do
        let tyVarNames = map tyVarName tyVarBndrs
        let typeConType = foldl AppT (ConT ty) (map VarT tyVarNames)
        resultTypeName <- newName "a"
        let args = map (conType ty resultTypeName) cons ++ [typeConType, VarT resultTypeName]
        return (SigD funName (ForallT (PlainTV resultTypeName : tyVarBndrs) [] (foldr1 makeFuncT args)))

    funDef :: [Con] -> Q Dec
    funDef cons = (FunD funName . (:[])) <$> funImpl cons

    funName :: Name
    funName = mkName $
        if null (cataName opts)
            then let (x:xs) = nameBase ty in toLower x : xs
            else cataName opts

    funImpl :: [Con] -> Q Clause
    funImpl cons = do
        conArgNames <- replicateM (length cons) (newName "c")

        valueArgName <- newName "x"
        let funArgs = map VarP (conArgNames ++ [valueArgName])

        matches <- forM (zip cons conArgNames) $ \(c, cn) -> do
            pat@(ConP _ conPats) <- conToConP c
            let patNames = map (\(VarP n) -> n) conPats

            let translateArg t arg = case typeName t of
                    Just n -> if n == ty then foldl AppE (VarE funName) (map VarE (conArgNames ++ [arg])) else VarE arg
                    Nothing -> VarE arg

            let argsWithTypes = zipWith translateArg (conArgTypes c) patNames
            let bodyE = foldl AppE (VarE cn) argsWithTypes
            return (Match pat (NormalB bodyE) [])

        let bodyE = CaseE (VarE valueArgName) matches
        return (Clause funArgs (NormalB bodyE) [])

      where
        conToConP :: Con -> Q Pat
        conToConP c = do
            argNames <- replicateM (length . conArgTypes $ c) (VarP <$> newName "a")
            return (ConP (conName c) argNames)

tyVarName :: TyVarBndr -> Name
tyVarName (PlainTV n)    = n
tyVarName (KindedTV n _) = n

