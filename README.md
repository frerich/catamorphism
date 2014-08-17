makemorphisms
=============

This project builds an Haskell module `Data.Morphism.Cata` which exports a
`makeCata` function which can be used to generate (catamorphisms)[http://www.haskell.org/haskellwiki/Catamorphisms]
for Haskell types. For instance, existing catamorphisms can be defined via

* (bool)[http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Bool.html#v:bool]:
``` haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Morphism.Cata

 -- generates 'bool :: a -> a -> Bool -> a
$(makeCata defaultOptions ''Bool)
```

* (maybe)[http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html#v:maybe]:
``` haskell
 -- generates maybe :: b -> (a -> b) -> Maybe a -> b
$(makeCata defaultOptions ''Maybe)
```

* (either)[http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Either.html#v:either]:
``` haskell
-- generates either :: (a -> c) -> (b -> c) -> Either a b -> c
$(makeCata defaultOptions ''Either)
```

* A custom fold over lists can be generated via; you can use the `cataName` field of the
options to assign a custom name:
``` haskell
$(makeCata defaultOptions { cataName = "fold" } ''[])
```

