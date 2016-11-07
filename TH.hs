{-# LANGUAGE TemplateHaskellQuotes #-} 
module TH where

import ClassyPlate

makeClassyPlate :: [Name] -> Name -> Q [Dec]
makeClassyPlate primitives dataType = undefined

containedTypes :: [Name] -> Dec -> Q [Dec]
containedTypes primitives dec@(DataD _ n [TyVarBndr] (Maybe Kind) [Con] Cxt)
  | n `elem` primitives = return []
  | otherwise 


getConArgs :: Con -> []



instance GoodOperation c => Apply c A where
  apply f (ABC b c) = app @(AppSelector c A) @c f $ ABC (apply @c f b) (apply @c f c)
  applyM f (ABC b c) = appM @(AppSelector c A) @c f =<< (ABC <$> applyM @c f b <*> applyM @c f c)
  applySelective f pred val@(ABC b c) = appIf @c f pred val (ABC (applySelective @c f pred b) (applySelective @c f pred c))
  applySelectiveM f pred val@(ABC b c) = appIfM @c f pred val (ABC <$> applySelectiveM @c f pred b <*> applySelectiveM @c f pred c)

