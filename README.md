makemorphisms
=============

This project builds an Haskell module `Data.Morphism.Cata` which exports a
`makeCata` function which can be used to generate [catamorphisms](http://www.haskell.org/haskellwiki/Catamorphisms)
for Haskell types. For instance, existing catamorphisms can be defined via

* [bool](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Bool.html#v:bool):
``` haskell
-- generates 'bool :: a -> a -> Bool -> a
$(makeCata defaultOptions ''Bool)
```

* [maybe](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html#v:maybe):
``` haskell
-- generates maybe :: b -> (a -> b) -> Maybe a -> b
$(makeCata defaultOptions ''Maybe)
```

* [either](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Either.html#v:either):
``` haskell
-- generates either :: (a -> c) -> (b -> c) -> Either a b -> c
$(makeCata defaultOptions ''Either)
```

However, catamorphisms are more useful for custom recursive data structures. For instance,
given a simple type for modelling expressions involving numbers, variables and sums as in

``` haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Morphism.Cata
import Data.Maybe (fromJust)
import Data.Function (on)

data Expr a = Number a
            | Variable Char
            | Sum (Expr a) (Expr a)
```

You can use the following `makeCata` invocation to generate a function for folding `Expr`
values - the function will be called `cataExpr`:

``` haskell
-- generates cataExpr :: (a -> b) -> (Char -> b) -> (Expr a -> Expr a -> b) -> Expr a -> b
$(makeCata defaultOptions { cataName = "cataExpr" } ''Expr)
```

This catamorphism can be used to define a whole bunch of other useful functions such as

``` haskell
-- Evaluate an Expr, given some variable bindings
eval :: Num a => [(Char, a)] -> Expr a -> a
eval vars = cataExpr id (fromJust . (`lookup` vars)) ((+) `on` eval vars)

-- Pretty-prints an Expr
pprint :: Show a => Expr a -> String
pprint = cataExpr show show (\a b -> pprint a ++ " + " ++ pprint b)

-- Counts the number of variables used in an expr
numVars :: Expr a -> Int
numVars = cataExpr (const 1) (const 0) ((+) `on` numVars)
``` haskell

