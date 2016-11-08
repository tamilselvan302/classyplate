{-# LANGUAGE Rank2Types, ConstraintKinds, KindSignatures, TypeFamilies, ScopedTypeVariables, MultiParamTypeClasses, AllowAmbiguousTypes
           , FlexibleContexts, FlexibleInstances, UndecidableInstances, DataKinds, TypeApplications, DeriveGeneric, DeriveDataTypeable
           , TypeOperators, PolyKinds, ScopedTypeVariables, TemplateHaskell #-}
module MiniLangExample where

import GHC.Exts
import Data.Maybe
import GHC.Generics (Generic)
import qualified GHC.Generics as GG
import Data.Data (Data)
import Control.Parallel.Strategies

import Language.Haskell.TH (pprint, runIO)
import Data.Type.Bool
import Data.Type.List hiding (Distinct)

import ClassyPlate
import MiniLanguage
import TH

testExpr :: Ann Expr Dom RangeStage
testExpr = Ann (NodeInfo NoInfo NodeSpan) $ Add 
             (Ann (NodeInfo NoInfo NodeSpan) $ Var (Ann (NodeInfo NameInfo NodeSpan) Name))
             (Ann (NodeInfo NoInfo NodeSpan) $ Var (Ann (NodeInfo NameInfo NodeSpan) Name))

class Transform t where
  trf :: t -> t

instance Transform (Ann Expr dom stage) where
  trf (Ann ann (Var n)) = (Ann ann (Add (Ann ann (Var n)) (Ann ann (Var n))))
  trf e = e

instance Transform (Ann Name dom stage) where
  trf e = e

type instance AppSelector Transform x = TransformAppSelector x

type family TransformAppSelector x where 
  TransformAppSelector (Ann Expr dom stage) = True
  TransformAppSelector (Ann Name dom stage) = True
  TransformAppSelector x = False


test = apply (undefined :: ClsToken Transform) trf $ testExpr

makeClassyPlate [ '_annotation ] ''Ann
makeClassyPlate [] ''Expr
makeClassyPlate [] ''Name


-- $( do pl <- makeClassyPlate [ '_annotation ] ''Ann
--       runIO $ putStrLn $ pprint pl
--       return [] )

-- instance (GoodOperationFor c (Ann e dom stage), Apply c (e dom stage)) => Apply c (Ann e dom stage) where
  -- apply t f (Ann ann e) = app (undefined :: FlagToken (AppSelector c (Ann e dom stage))) t f $ Ann ann (apply t f e)
  -- applyM t f (Ann ann e) = appM (undefined :: FlagToken (AppSelector c (Ann e dom stage))) t f =<< (Ann <$> return ann <*> applyM t f e)


-- instance (GoodOperationFor c (Expr dom stage), Apply c (Ann Name dom stage)) => Apply c (Expr dom stage) where
  -- apply t f (Add e1 e2) = app (undefined :: FlagToken (AppSelector c (Expr dom stage))) t f $ Add (apply t f e1) (apply t f e2)

  -- applyM f (ABC b c) = appM @(AppSelector c (A x)) @c f =<< (ABC <$> applyM @c f b <*> applyM @c f c)
  -- applySelective f pred val@(ABC b c) = appIf @c f pred val (ABC (applySelective @c f pred b) (applySelective @c f pred c))
  -- applySelectiveM f pred val@(ABC b c) = appIfM @c f pred val (ABC <$> applySelectiveM @c f pred b <*> applySelectiveM @c f pred c)

-- instance (GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False (A x) where
  -- applyAuto f (ABC b c) = app @(AppSelector c A) @c f $ ABC (applyAuto @c @(ClassIgnoresSubtree c B) f b) (applyAuto @c @(ClassIgnoresSubtree c C) f c)
  -- applyAutoM f (ABC b c) = appM @(AppSelector c A) @c f =<< (ABC <$> applyAutoM @c @(ClassIgnoresSubtree c B) f b <*> applyAutoM @c @(ClassIgnoresSubtree c C) f c)

-- instance (GoodOperationFor c (Name dom stage)) => Apply c (Name dom stage) where
  -- apply t f Name = app (undefined :: FlagToken (AppSelector c (Name dom stage))) t f Name
