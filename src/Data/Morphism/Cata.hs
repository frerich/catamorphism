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
such as 'bool' (for Bool values), 'maybe' (for Maybe values) values, 'either' for (Either values)
values and foldr (for lists). These catamorphisms could have been generated using
'makeCata' as follows:

> -- Defines 'bool :: a -> a -> Bool -> a'
> $(makeCata defaultOptions ''Bool)
>
> -- Defines 'maybe :: b -> (Maybe a -> b) -> Maybe a -> b'
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
> import Data.Function (on)
> 
> data Expr a = Number a
>             | Variable Char
>             | Sum (Expr a) (Expr a)
> 
> makeCata defaultOptions { cataName = "cataExpr" } ''Expr

The 'makeCata' invocation defines a 'cataExpr' function which works like a fold on
'Expr' values; it can be used to implement various useful other functions:

> -- Evaluate an Expr, given some variable bindings
> eval :: Num a => [(Char, a)] -> Expr a -> a
> eval vars = cataExpr id (fromJust . (`lookup` vars)) ((+) `on` eval vars)
> 
> -- Pretty-prints an Expr
> pprint :: Show a => Expr a -> String
> pprint = cataExpr show show (\a b -> pprint a ++ " + " ++ pprint b)
> 
> -- Counts the number of variables used in an expr
> numVars :: Expr a -> Int
> numVars = cataExpr (const 1) (const 0) ((+) `on` numVars)
-}

module Data.Morphism.Cata
    ( CataOptions(..)
    , defaultOptions
    , makeCata
    )
where

import Control.Monad (replicateM)

import Data.Char (toLower)

import Language.Haskell.TH

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

makeFuncT :: TypeQ -> TypeQ -> TypeQ
makeFuncT a = appT (appT arrowT a)

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

conType :: Name -> Con -> TypeQ
conType resultT c = foldr makeFuncT (varT resultT) (map return (conArgTypes c))

-- |The 'makeCata' function creates a catamorphism for the given type.
makeCata :: CataOptions     -- Options to customize the catamorphism; the name of the defined function can be changed
         -> Name            -- The type to generate a catamorphism for.
         -> DecsQ
makeCata opts typeName = do
  info          <- reify typeName
  (bndrs, cons) <- case info of
     TyConI (DataD    _ _ bndrs cons _) -> return (bndrs, cons)
     TyConI (NewtypeD _ _ bndrs con  _) -> return (bndrs, [con])
     _                                  -> fail "makeCata: Expected name of type constructor"
  sequence [signature bndrs cons, funDef cons]

  where
    signature :: [TyVarBndr] -> [Con] -> DecQ
    signature tyVarBndrs cons = do
        resultTypeName <- newName "a"
        let tyVarNames  = map tvName tyVarBndrs
            typeConType = appsT (conT typeName) (map varT tyVarNames)
            args        = map (conType resultTypeName) cons ++ [typeConType, varT resultTypeName]
        sigD funName (forallT (PlainTV resultTypeName : tyVarBndrs) (cxt[]) (foldr1 makeFuncT args))

    funDef :: [Con] -> DecQ
    funDef cons = funD funName [funImpl cons]

    funName :: Name
    funName = mkName $
        if null (cataName opts)
            then let (x:xs) = nameBase typeName in toLower x : xs
            else cataName opts

    funImpl :: [Con] -> ClauseQ
    funImpl cons = do
        conArgs      <- replicateM (length cons) (newName "c")
        valueArgName <- newName "x"

        let funArgs = map varP conArgs ++ [varP valueArgName]

            mkMatch c cn = do
              argNames <- replicateM (length (conArgTypes c)) (newName "a")
              let pat       = conP (conName c) (map varP argNames)
              let matchBody = appsE (map varE (cn:argNames))
              match pat (normalB matchBody) []

            bodyE = caseE (varE valueArgName) (zipWith mkMatch cons conArgs)

        clause funArgs (normalB bodyE) []

tvName :: TyVarBndr -> Name
tvName (PlainTV n) = n
tvName (KindedTV n _) = n

appsT :: TypeQ -> [TypeQ] -> TypeQ
appsT = foldl appT
