{-# LANGUAGE TemplateHaskellQuotes #-} 
module TH where

import Data.Maybe
import Control.Monad
import Control.Applicative

import Language.Haskell.TH

import ClassyPlate

-- | Creates ClassyPlate instances for a datatype. Can specify which fields should not be traversed.
makeClassyPlate :: [Name] -> Name -> Q [Dec]
makeClassyPlate primitives dataType 
  = do inf <- reify dataType
       case inf of (TyConI (DataD _ name tvs _ cons _)) 
                     -> return $ [makeCPForDataType name tvs (map (getConRep primitives) cons)]

makeCPForDataType :: Name -> [TyVarBndr] -> [ConRep] -> Dec
makeCPForDataType name tvs cons
  = let headType = foldl AppT (ConT name) (map (VarT . getTVName) tvs)
        clsVar = mkName "c"
     in InstanceD Nothing (generateCtx clsVar headType cons) 
                          (ConT ''Apply `AppT` VarT clsVar `AppT` headType) 
                          (generateDefs clsVar headType name cons)

generateCtx :: Name -> Type -> [ConRep] -> Cxt
generateCtx clsVar selfType cons 
  = (ConT ''GoodOperationFor `AppT` VarT clsVar `AppT` selfType) 
      : map ((ConT ''Apply `AppT` VarT clsVar) `AppT`) (concatMap (\(_, args) -> catMaybes args) cons)

generateDefs :: Name -> Type -> Name -> [ConRep] -> [Dec]
generateDefs clsVar headType tyName cons = 
  [ FunD 'apply (map (generateAppClause clsVar headType tyName) cons)
  , FunD 'applyM (map (generateAppMClause clsVar headType tyName) cons)
  , FunD 'applySelective (map (generateSelectiveAppClause tyName) cons)
  , FunD 'applySelectiveM (map (generateSelectiveAppMClause tyName) cons)
  ]

-- | Creates the clause for the @apply@ function for one constructor: @apply t f (Add e1 e2) = app (undefined :: FlagToken (AppSelector c (Expr dom stage))) t f $ Add (apply t f e1) (apply t f e2)@
generateAppClause :: Name -> Type -> Name -> ConRep -> Clause
generateAppClause clsVar headType tyName (conName, args) 
  = Clause [VarP tokenName, VarP funName, ConP conName (map VarP $ take (length args) argNames)] 
      (NormalB (generateAppExpr clsVar headType tokenName funName 
                 `AppE` generateRecombineExpr conName tokenName funName (zip (map isJust args) argNames))) []
  where argNames = map (mkName . ("a"++) . show) [0..]
        tokenName = mkName "t"
        funName = mkName "f"

generateAppExpr :: Name -> Type -> Name -> Name -> Exp
generateAppExpr clsVar headType tokenName funName
  = VarE 'app `AppE` (VarE 'undefined `SigE` (ConT ''FlagToken `AppT` (ConT ''AppSelector `AppT` VarT clsVar `AppT` headType)))
              `AppE` VarE tokenName `AppE` VarE funName

generateRecombineExpr :: Name -> Name -> Name -> [(Bool, Name)] -> Exp
generateRecombineExpr conName tokenName funName args
  = foldl AppE (ConE conName) (map mapArgRep args)
  where mapArgRep (True, n) = VarE 'apply `AppE` VarE tokenName `AppE` VarE funName `AppE` VarE n
        mapArgRep (False, n) = VarE n

-- | Creates the clause for the @applyM@ function for one constructor: @applyM t f (Ann ann e) = appM (undefined :: FlagToken (AppSelector c (Ann e dom stage))) t f =<< (Ann <$> return ann <*> applyM t f e)@
generateAppMClause :: Name -> Type -> Name -> ConRep -> Clause
generateAppMClause clsVar headType tyName (conName, args) 
  = Clause [VarP tokenName, VarP funName, ConP conName (map VarP $ take (length args) argNames)] 
      (NormalB (InfixE (Just $ generateAppMExpr clsVar headType tokenName funName)
                       (VarE '(=<<))
                       (Just $ generateRecombineMExpr conName tokenName funName (zip (map isJust args) argNames)) )) []
  where argNames = map (mkName . ("a"++) . show) [0..]
        tokenName = mkName "t"
        funName = mkName "f"

generateAppMExpr :: Name -> Type -> Name -> Name -> Exp
generateAppMExpr clsVar headType tokenName funName
  = VarE 'appM `AppE` (VarE 'undefined `SigE` (ConT ''FlagToken `AppT` (ConT ''AppSelector `AppT` VarT clsVar `AppT` headType)))
               `AppE` VarE tokenName `AppE` VarE funName

generateRecombineMExpr :: Name -> Name -> Name -> [(Bool, Name)] -> Exp
generateRecombineMExpr conName tokenName funName []
  = AppE (VarE 'return) (ConE conName)
generateRecombineMExpr conName tokenName funName (fst:args)
  = foldl (\base -> InfixE (Just base) (VarE '(<*>)) . Just) 
          (InfixE (Just $ ConE conName) (VarE '(<$>)) (Just $ mapArgRep fst)) 
          (map mapArgRep args)
  where mapArgRep (True, n) = VarE 'applyM `AppE` VarE tokenName `AppE` VarE funName `AppE` VarE n
        mapArgRep (False, n) = VarE 'return `AppE` VarE n

-- | Creates the clause for the @applySelective@ function for one constructor: @applySelective t f pred val@(CB b) = appIf t f pred val (CB (applySelective t f pred b))@
generateSelectiveAppClause :: Name -> ConRep -> Clause
generateSelectiveAppClause tyName (conName, args) 
  = Clause [VarP tokenName, VarP funName, VarP predName, AsP valName $ ConP conName (map VarP $ take (length args) argNames)] 
      (NormalB (generateAppIfExpr tokenName funName predName valName
                 `AppE` generateSelectiveRecombineExpr conName tokenName funName predName (zip (map isJust args) argNames))) []
  where argNames = map (mkName . ("a"++) . show) [0..]
        tokenName = mkName "t"
        funName = mkName "f"
        predName = mkName "p"
        valName = mkName "v"

generateAppIfExpr :: Name -> Name -> Name -> Name -> Exp
generateAppIfExpr tokenName funName predName valName
  = VarE 'appIf `AppE` VarE tokenName `AppE` VarE funName `AppE` VarE predName `AppE` VarE valName

generateSelectiveRecombineExpr :: Name -> Name -> Name -> Name -> [(Bool, Name)] -> Exp
generateSelectiveRecombineExpr conName tokenName funName predName args
  = foldl AppE (ConE conName) (map mapArgRep args)
  where mapArgRep (True, n) = VarE 'applySelective `AppE` VarE tokenName `AppE` VarE funName `AppE` VarE predName `AppE` VarE n
        mapArgRep (False, n) = VarE n

-- | Creates the clause for the @applySelective@ function for one constructor: @applySelective t f pred val@(CB b) = appIf t f pred val (CB (applySelective t f pred b))@
generateSelectiveAppMClause :: Name -> ConRep -> Clause
generateSelectiveAppMClause tyName (conName, args) 
  = Clause [VarP tokenName, VarP funName, VarP predName, AsP valName $ ConP conName (map VarP $ take (length args) argNames)] 
      (NormalB (generateAppIfMExpr tokenName funName predName valName
                 `AppE` generateSelectiveRecombineMExpr conName tokenName funName predName (zip (map isJust args) argNames))) []
  where argNames = map (mkName . ("a"++) . show) [0..]
        tokenName = mkName "t"
        funName = mkName "f"
        predName = mkName "p"
        valName = mkName "v"

generateAppIfMExpr :: Name -> Name -> Name -> Name -> Exp
generateAppIfMExpr tokenName funName predName valName
  = VarE 'appIfM `AppE` VarE tokenName `AppE` VarE funName `AppE` VarE predName `AppE` VarE valName

generateSelectiveRecombineMExpr :: Name -> Name -> Name -> Name -> [(Bool, Name)] -> Exp
generateSelectiveRecombineMExpr conName tokenName funName predName []
  = AppE (VarE 'return) (ConE conName)
generateSelectiveRecombineMExpr conName tokenName funName predName (fst:args)
  = foldl (\base -> InfixE (Just base) (VarE '(<*>)) . Just) 
          (InfixE (Just $ ConE conName) (VarE '(<$>)) (Just $ mapArgRep fst)) 
          (map mapArgRep args)
  where mapArgRep (True, n) = VarE 'applySelectiveM `AppE` VarE tokenName `AppE` VarE funName `AppE` VarE predName `AppE` VarE n
        mapArgRep (False, n) = VarE 'return `AppE` VarE n



getTVName :: TyVarBndr -> Name
getTVName (PlainTV n) = n
getTVName (KindedTV n _) = n


type ConRep = (Name, [Maybe Type])

getConRep :: [Name] -> Con -> ConRep
getConRep primitives (NormalC n args) = (n, map (Just . snd) args)
getConRep primitives (RecC n args) = (n, map (\(fldN,_,t) -> if fldN `elem` primitives then Nothing else Just t) args)
getConRep primitives (InfixC (_,t1) n (_,t2)) = (n, [Just t1, Just t2])
-- getConArgs (ForallC _ _ c) = getConArgs c
-- getConArgs (GadtC _ args _) = map snd args
-- getConArgs (RecGadtC _ args _) = map (\(_,_,t) -> t) args


-- instance GoodOperation c => Apply c A where
--   apply f (ABC b c) = app @(AppSelector c A) @c f $ ABC (apply @c f b) (apply @c f c)
--   applyM f (ABC b c) = appM @(AppSelector c A) @c f =<< (ABC <$> (applyM @c f b) <*> (applyM @c f c))
--   applySelective f pred val@(ABC b c) = appIf @c f pred val (ABC (applySelective @c f pred b) (applySelective @c f pred c))
--   applySelectiveM f pred val@(ABC b c) = appIfM @c f pred val (ABC <$> (applySelectiveM @c f pred b) <*> (applySelectiveM @c f pred c))

