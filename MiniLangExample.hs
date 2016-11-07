{-# LANGUAGE Rank2Types, ConstraintKinds, KindSignatures, TypeFamilies, ScopedTypeVariables, MultiParamTypeClasses, AllowAmbiguousTypes
           , FlexibleContexts, FlexibleInstances, UndecidableInstances, DataKinds, TypeApplications, DeriveGeneric, DeriveDataTypeable
           , TypeOperators, PolyKinds, ScopedTypeVariables #-}
module Example where

import GHC.Exts
import Data.Maybe
import GHC.Generics (Generic)
import qualified GHC.Generics as GG
import Data.Data (Data)
import Control.Parallel.Strategies

import Data.Type.Bool
import Data.Type.List hiding (Distinct)

import ClassyPlate
import MiniLanguage
-- import TH

testExpr :: Ann Expr Dom RangeStage
testExpr = Ann (NodeInfo NoInfo NodeSpan) $ Add (Ann (NodeInfo NameInfo NodeSpan) Name) (Ann (NodeInfo NameInfo NodeSpan) Name)

class Transform t where
  trf :: t -> t

instance Transform (Ann Expr dom stage) where
  trf e = e

instance Transform (Ann Name dom stage) where
  trf e = e

type instance AppSelector Transform x = TransformAppSelector x

type family TransformAppSelector x where 
  TransformAppSelector (Ann Expr dom stage) = True
  TransformAppSelector (Ann Name dom stage) = True
  TransformAppSelector x = False


test = apply @Transform trf $ testExpr

-- type GoodOperationFor c e = (App (AppSelector c e) c e, AutoApply c (ClassIgnoresSubtree c e) e)
type GoodOperationFor c e = (App (AppSelector c e) c e)

instance (GoodOperationFor c (Ann e dom stage), Apply c (e dom stage)) => Apply c (Ann e dom stage) where
  apply f (Ann ann e) = app @(AppSelector c (Ann e dom stage)) @c f $ Ann ann (apply @c f e)


instance (GoodOperationFor c (Expr dom stage), Apply c (Ann Name dom stage)) => Apply c (Expr dom stage) where
  apply f (Add e1 e2) = app @(AppSelector c (Expr dom stage)) @c f $ Add (apply @c f e1) (apply @c f e2)

  -- applyM f (ABC b c) = appM @(AppSelector c (A x)) @c f =<< (ABC <$> applyM @c f b <*> applyM @c f c)
  -- applySelective f pred val@(ABC b c) = appIf @c f pred val (ABC (applySelective @c f pred b) (applySelective @c f pred c))
  -- applySelectiveM f pred val@(ABC b c) = appIfM @c f pred val (ABC <$> applySelectiveM @c f pred b <*> applySelectiveM @c f pred c)

-- instance (GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False (A x) where
--   applyAuto f (ABC b c) = app @(AppSelector c A) @c f $ ABC (applyAuto @c @(ClassIgnoresSubtree c B) f b) (applyAuto @c @(ClassIgnoresSubtree c C) f c)
--   applyAutoM f (ABC b c) = appM @(AppSelector c A) @c f =<< (ABC <$> applyAutoM @c @(ClassIgnoresSubtree c B) f b <*> applyAutoM @c @(ClassIgnoresSubtree c C) f c)

instance (GoodOperationFor c (Name dom stage)) => Apply c (Name dom stage) where
  apply f Name = app @(AppSelector c (Name dom stage)) @c f Name
