{-# LANGUAGE Rank2Types, ConstraintKinds, KindSignatures, TypeFamilies, ScopedTypeVariables, MultiParamTypeClasses, AllowAmbiguousTypes
           , FlexibleContexts, FlexibleInstances, UndecidableInstances, DataKinds, TypeApplications, DeriveGeneric, DeriveDataTypeable
           , TypeOperators, PolyKinds, ScopedTypeVariables, TemplateHaskell #-}
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
import TH


-------------------------------- USAGE-SPECIFIC PART

test :: A
test = apply @F undefined trf $ ABC (BA (ABC B (CB B))) (CB B)

class F a where
  trf :: a -> a

instance F B where
  trf b = B

instance F C where
  trf c = CB B
  
type instance AppSelector F A = 'False
type instance AppSelector F B = 'True
type instance AppSelector F C = 'True
type instance AppSelector F D = 'False
type instance AppSelector F E = 'False

-- type instance SelectedElements F = '[ B, C ]

--------

test2 = applyM @Debug undefined debugCs $ ABC (BA (ABC B (CB B))) (CD D)

class Debug a where
  debugCs :: a -> IO a

instance Debug C where
  debugCs c = print c >> return c
  
type instance AppSelector Debug a = DebugSelector a

-- type instance SelectedElements Debug = '[ C ]

type family DebugSelector t where
  DebugSelector C = 'True
  DebugSelector _ = 'False

--------

test3 :: A
test3 = apply @(MonoMatch C) undefined (monoApp (\c -> case c of CB b -> CB (BA (ABC b (CB B))); CD d -> CD D)) $ ABC (BA (ABC B (CB B))) (CB B)

type instance SelectedElements (MonoMatch t) = '[ t ]

-------

class DebugWhere a where
  debugWhereCs :: a -> IO a
  debugSubtree :: a -> Bool
  debugSubtree _ = True

instance DebugWhere C where
  debugWhereCs c = print c >> return c

instance DebugWhere D where
  debugWhereCs c = return c
  debugSubtree _ = False

instance DebugWhere E where
  debugWhereCs c = error "Should never go here" >> return c 

type instance AppSelector DebugWhere a = DebugWhereSelector a

-- type instance SelectedElements DebugWhere = '[ forall x . C x, D, E ]

type family DebugWhereSelector t where
  DebugWhereSelector C = 'True
  DebugWhereSelector D = 'True
  DebugWhereSelector E = 'True
  DebugWhereSelector _ = 'False

test4 = applySelectiveM @DebugWhere undefined debugWhereCs (return . debugSubtree) $ ABC (BA (ABC B (CB B))) (CD (DDE D E))


-------------------------------- REPRESENTATION-SPECIFIC PART

data A = ABC B C deriving (Show, Generic, Data)
data B = B | BA A deriving (Show, Generic, Data)
data C = CB B | CD D deriving (Show, Generic, Data)
data D = DDE D E | D deriving (Show, Generic, Data)
data E = E deriving (Show, Generic, Data)

instance NFData A
instance NFData B
instance NFData C
instance NFData D
instance NFData E

makeClassyPlate [] ''A
makeClassyPlate [] ''B
makeClassyPlate [] ''C
makeClassyPlate [] ''D
makeClassyPlate [] ''E

-- instance (GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False (A x) where
--   applyAuto f (ABC b c) = app @(AppSelector c A) @c f $ ABC (applyAuto @c @(ClassIgnoresSubtree c B) f b) (applyAuto @c @(ClassIgnoresSubtree c C) f c)
--   applyAutoM f (ABC b c) = appM @(AppSelector c A) @c f =<< (ABC <$> applyAutoM @c @(ClassIgnoresSubtree c B) f b <*> applyAutoM @c @(ClassIgnoresSubtree c C) f c)

-- instance (GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C D), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False B where
--   applyAuto f B = app @(AppSelector c B) @c f B
--   applyAuto f (BA a) = app @(AppSelector c B) @c f $ BA (applyAuto @c @(ClassIgnoresSubtree c A) f a)

--   applyAutoM f B = appM @(AppSelector c B) @c f B
--   applyAutoM f (BA a) = appM @(AppSelector c B) @c f =<< (BA <$> applyAutoM @c @(ClassIgnoresSubtree c A) f a)

-- instance (GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False (C x) where
--   applyAuto f (CB b) = app @(AppSelector c (C x)) @c f $ CB (applyAuto @c @(ClassIgnoresSubtree c B) f b)
--   applyAuto f (CD d) = app @(AppSelector c (C x)) @c f $ CD (applyAuto @c @(ClassIgnoresSubtree c D) f d)

--   applyAutoM f (CB b) = appM @(AppSelector c (C x)) @c f =<< (CB <$> applyAutoM @c @(ClassIgnoresSubtree c B) f b)
--   applyAutoM f (CD d) = appM @(AppSelector c (C x)) @c f =<< (CD <$> applyAutoM @c @(ClassIgnoresSubtree c D) f d)

-- instance (GoodOperationFor c E, GoodOperationFor c D) => AutoApply c False D where
--   applyAuto f (DDE d e) = app @(AppSelector c D) @c f $ DDE (applyAuto @c @(ClassIgnoresSubtree c D) f d) (applyAuto @c @(ClassIgnoresSubtree c E) f e)
--   applyAuto f D = app @(AppSelector c D) @c f $ D

--   applyAutoM f (DDE d e) = appM @(AppSelector c D) @c f =<< (DDE <$> applyAutoM @c @(ClassIgnoresSubtree c D) f d <*> applyAutoM @c @(ClassIgnoresSubtree c E) f e)
--   applyAutoM f D = appM @(AppSelector c D) @c f =<< (return D)

-- instance (GoodOperationFor c E) => AutoApply c False E where
--   applyAuto f E = app @(AppSelector c E) @c f E
--   applyAutoM f E = appM @(AppSelector c E) @c f E

