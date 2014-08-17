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

data CataOptions = CataOptions {
    cataName :: String
}

defaultOptions :: CataOptions
defaultOptions = CataOptions ""

makeFuncT :: Type -> Type -> Type
makeFuncT a = AppT (AppT ArrowT a)

conArgTypes :: Con -> [Type]
conArgTypes (NormalC _ args) = map snd args
conArgTypes (RecC _ args) = map (\(_,_,x) -> x) args
conArgTypes (InfixC arg1 _ arg2) = map snd [arg1, arg2]
conArgTypes (ForallC _ _ c) = conArgTypes c

conName :: Con -> Name
conName (NormalC n _) = n
conName (RecC n _) = n
conName (InfixC _ n _) = n
conName (ForallC _ _ c) = conName c

conType :: Name -> Con -> Type
conType resultT c = foldr makeFuncT (VarT resultT) (conArgTypes c)

makeCata :: CataOptions -> Name -> Q [Dec]
makeCata (CataOptions cataName) typeName  = sequence [signature, funDef]
  where
    signature :: Q Dec
    signature = do
        (TyConI (DataD _ _ tyVarBndrs cons _)) <- reify typeName
        let tyVarNames = map (\(PlainTV n) -> n) tyVarBndrs
        let conT = foldl AppT (ConT typeName) (map VarT tyVarNames)
        resultTypeName <- newName "a"
        let args = map (conType resultTypeName) cons ++ [conT, VarT resultTypeName]
        return (SigD funName (ForallT (PlainTV resultTypeName : tyVarBndrs) [] (foldr1 makeFuncT args)))

    funDef :: Q Dec
    funDef = (FunD funName . (:[])) <$> funImpl

    funName :: Name
    funName = mkName $
        if null cataName
            then let (x:xs) = nameBase typeName in toLower x : xs
            else cataName

    funImpl :: Q Clause
    funImpl = do
        (TyConI (DataD _ _ _ cons _)) <- reify typeName
        conArgs <- replicateM (length cons) (VarP <$> newName "c")

        valueArgName <- newName "x"
        let funArgs = conArgs ++ [VarP valueArgName]

        matches <- forM (zip cons conArgs) $ \(c, VarP cn) -> do
            pat@(ConP _ conPats) <- conToConP c
            let patNames = map (\(VarP n) -> n) conPats

            let bodyE = foldl1 AppE . map VarE $ (cn : patNames)
            return (Match pat (NormalB bodyE) [])

        let bodyE = CaseE (VarE valueArgName) matches
        return (Clause funArgs (NormalB bodyE) [])

      where
        conToConP :: Con -> Q Pat
        conToConP c = do
            argNames <- replicateM (length . conArgTypes $ c) (VarP <$> newName "a")
            return (ConP (conName c) argNames)

