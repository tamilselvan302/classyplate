{-# LANGUAGE TemplateHaskellQuotes #-} 
module TH where

import Data.Maybe

import Language.Haskell.TH

import ClassyPlate

makeClassyPlate :: [Name] -> Name -> Q [Dec]
makeClassyPlate primitives dataType 
  = do inf <- reify dataType
       case inf of (TyConI (DataD _ name tvs _ cons _)) -> return $ [makeCPForDataType primitives name tvs cons]

makeCPForDataType :: [Name] -> Name -> [TyVarBndr] -> [Con] -> Dec
makeCPForDataType primitives name tvs cons
  = let headType = foldl AppT (ConT name) (map (VarT . getTVName) tvs)
     in InstanceD Nothing (generateCtx primitives headType cons) headType (generateDefs primitives name cons)

generateCtx :: [Name] -> Type -> [Con] -> Cxt
generateCtx primitives selfType cons = undefined

generateDefs :: [Name] -> Name -> [Con] -> [Dec]
generateDefs primitives tyName cons = [FunD 'apply (map (generateAppClause tyName . getConRep primitives) cons)]

generateAppClause :: Name -> ConRep -> Clause
generateAppClause tyName (conName, args) 
  = Clause (map VarP $ take (length args) argNames) (NormalB (generateAppExpr tyName conName (zip (map isJust args) argNames))) []
  where argNames = map (mkName . ("a"++) . show) [0..]

generateAppExpr :: Name -> Name -> [(Bool, Name)] -> Exp
generateAppExpr tyName conName args
  = undefined

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

