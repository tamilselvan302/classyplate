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

instance (GoodOperationForAuto c A, GoodOperationForAuto c B, GoodOperationForAuto c C, GoodOperationForAuto c D, GoodOperationForAuto c E) => AutoApply c False A where
  applyAuto t f (ABC b c) = app @(AppSelector c A) undefined t f $ ABC (applyAuto @c @(ClassIgnoresSubtree c B) t f b) (applyAuto @c @(ClassIgnoresSubtree c C) t f c)
  applyAutoM t f (ABC b c) = appM @(AppSelector c A) undefined t f =<< (ABC <$> applyAutoM @c @(ClassIgnoresSubtree c B) t f b <*> applyAutoM @c @(ClassIgnoresSubtree c C) t f c)

instance (GoodOperationForAuto c A, GoodOperationForAuto c B, GoodOperationForAuto c C, GoodOperationForAuto c D, GoodOperationForAuto c E) => AutoApply c False B where
  applyAuto t f B = app @(AppSelector c B) undefined t f B
  applyAuto t f (BA a) = app @(AppSelector c B) undefined t f $ BA (applyAuto @c @(ClassIgnoresSubtree c A) t f a)

  applyAutoM t f B = appM @(AppSelector c B) undefined t f B
  applyAutoM t f (BA a) = appM @(AppSelector c B) undefined t f =<< (BA <$> applyAutoM @c @(ClassIgnoresSubtree c A) t f a)

instance (GoodOperationForAuto c A, GoodOperationForAuto c B, GoodOperationForAuto c C, GoodOperationForAuto c D, GoodOperationForAuto c E) => AutoApply c False C where
  applyAuto t f (CB b) = app @(AppSelector c C) undefined t f $ CB (applyAuto @c @(ClassIgnoresSubtree c B) t f b)
  applyAuto t f (CD d) = app @(AppSelector c C) undefined t f $ CD (applyAuto @c @(ClassIgnoresSubtree c D) t f d)

  applyAutoM t f (CB b) = appM @(AppSelector c C) undefined t f =<< (CB <$> applyAutoM @c @(ClassIgnoresSubtree c B) t f b)
  applyAutoM t f (CD d) = appM @(AppSelector c C) undefined t f =<< (CD <$> applyAutoM @c @(ClassIgnoresSubtree c D) t f d)

instance (GoodOperationForAuto c E, GoodOperationForAuto c D) => AutoApply c False D where
  applyAuto t f (DDE d e) = app @(AppSelector c D) undefined t f $ DDE (applyAuto @c @(ClassIgnoresSubtree c D) t f d) (applyAuto @c @(ClassIgnoresSubtree c E) t f e)
  applyAuto t f D = app @(AppSelector c D) undefined t f $ D

  applyAutoM t f (DDE d e) = appM @(AppSelector c D) undefined t f =<< (DDE <$> applyAutoM @c @(ClassIgnoresSubtree c D) t f d <*> applyAutoM @c @(ClassIgnoresSubtree c E) t f e)
  applyAutoM t f D = appM @(AppSelector c D) undefined t f =<< (return D)

instance (GoodOperationForAuto c E) => AutoApply c False E where
  applyAuto t f E = app @(AppSelector c E) undefined t f E
  applyAutoM t f E = appM @(AppSelector c E) undefined t f E

