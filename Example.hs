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
-- import TH


-------------------------------- USAGE-SPECIFIC PART

test :: A D
test = apply @F trf $ ABC (BA (ABC B (CB B))) (CB B)

class F a where
  trf :: a -> a

instance F B where
  trf b = B

instance F (C x) where
  trf c = CB B
  
type instance AppSelector F (A x) = 'False
type instance AppSelector F B = 'True
type instance AppSelector F (C x) = 'True
type instance AppSelector F D = 'False
type instance AppSelector F E = 'False

-- type instance SelectedElements F = '[ B, C ]

--------

test2 = applyM @Debug debugCs $ ABC (BA (ABC B (CB B))) (CD D)

class Debug a where
  debugCs :: a -> IO a

instance Show x => Debug (C x) where
  debugCs c = print c >> return c
  
type instance AppSelector Debug a = DebugSelector a

-- type instance SelectedElements Debug = '[ C ]

type family DebugSelector t where
  DebugSelector (C x) = 'True
  DebugSelector _ = 'False

--------

test3 :: A D
test3 = apply @(MonoMatch (C D)) (monoApp (\c -> case c of CB b -> CB (BA (ABC b (CB B))); CD d -> CD D)) $ ABC (BA (ABC B (CB B))) (CB B)

type instance SelectedElements (MonoMatch t) = '[ t ]

-------

class DebugWhere a where
  debugWhereCs :: a -> IO a
  debugSubtree :: a -> Bool
  debugSubtree _ = True

instance Show x => DebugWhere (C x) where
  debugWhereCs c = print c >> return c

instance DebugWhere D where
  debugWhereCs c = return c
  debugSubtree _ = False

instance DebugWhere E where
  debugWhereCs c = error "Should never go here" >> return c 

type instance AppSelector DebugWhere a = DebugWhereSelector a

-- type instance SelectedElements DebugWhere = '[ forall x . C x, D, E ]

type family DebugWhereSelector t where
  DebugWhereSelector (C x) = 'True
  DebugWhereSelector D = 'True
  DebugWhereSelector E = 'True
  DebugWhereSelector _ = 'False

test4 = applySelectiveM @DebugWhere debugWhereCs (return . debugSubtree) $ ABC (BA (ABC B (CB B))) (CD (DDE D E))


-------------------------------- REPRESENTATION-SPECIFIC PART

data A x = ABC B (C x) deriving (Show, Generic, Data)
data B = B | BA (A D) deriving (Show, Generic, Data)
data C x = CB B | CD x deriving (Show, Generic, Data)
data D = DDE D E | D deriving (Show, Generic, Data)
data E = E deriving (Show, Generic, Data)

instance NFData x => NFData (A x)
instance NFData B
instance NFData x => NFData (C x)
instance NFData D
instance NFData E

-- type GoodOperationFor c e = (App (AppSelector c e) c e, AutoApply c (ClassIgnoresSubtree c e) e)
type GoodOperationFor c e = (App (AppSelector c e) c e)

instance (Apply c x, GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c (C D), GoodOperationFor c D, GoodOperationFor c E) => Apply c (A x) where
  apply f (ABC b c) = app @(AppSelector c (A x)) @c f $ ABC (apply @c f b) (apply @c f c)
  applyM f (ABC b c) = appM @(AppSelector c (A x)) @c f =<< (ABC <$> applyM @c f b <*> applyM @c f c)
  applySelective f pred val@(ABC b c) = appIf @c f pred val (ABC (applySelective @c f pred b) (applySelective @c f pred c))
  applySelectiveM f pred val@(ABC b c) = appIfM @c f pred val (ABC <$> applySelectiveM @c f pred b <*> applySelectiveM @c f pred c)

-- instance (GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False (A x) where
--   applyAuto f (ABC b c) = app @(AppSelector c A) @c f $ ABC (applyAuto @c @(ClassIgnoresSubtree c B) f b) (applyAuto @c @(ClassIgnoresSubtree c C) f c)
--   applyAutoM f (ABC b c) = appM @(AppSelector c A) @c f =<< (ABC <$> applyAutoM @c @(ClassIgnoresSubtree c B) f b <*> applyAutoM @c @(ClassIgnoresSubtree c C) f c)

instance (GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C D), GoodOperationFor c D, GoodOperationFor c E) => Apply c B where
  apply f B = app @(AppSelector c B) @c f B
  apply f (BA a) = app @(AppSelector c B) @c f $ BA (apply @c f a)

  applyM f B = appM @(AppSelector c B) @c f B
  applyM f (BA a) = appM @(AppSelector c B) @c f =<< (BA <$> applyM @c f a)

  applySelective f pred val@B = appIf @c f pred val B
  applySelective f pred val@(BA a) = appIf @c f pred val (BA (applySelective @c f pred a))

  applySelectiveM f pred val@B = appIfM @c f pred val (return B)
  applySelectiveM f pred val@(BA a) = appIfM @c f pred val (BA <$> applySelectiveM @c f pred a)

-- instance (GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C D), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False B where
--   applyAuto f B = app @(AppSelector c B) @c f B
--   applyAuto f (BA a) = app @(AppSelector c B) @c f $ BA (applyAuto @c @(ClassIgnoresSubtree c A) f a)

--   applyAutoM f B = appM @(AppSelector c B) @c f B
--   applyAutoM f (BA a) = appM @(AppSelector c B) @c f =<< (BA <$> applyAutoM @c @(ClassIgnoresSubtree c A) f a)

instance (Apply c x, GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c (C D), GoodOperationFor c D, GoodOperationFor c E) => Apply c (C x) where
  apply f (CB b) = app @(AppSelector c (C x)) @c f $ CB (apply @c f b)
  apply f (CD d) = app @(AppSelector c (C x)) @c f $ CD (apply @c f d)

  applyM f (CB b) = appM @(AppSelector c (C x)) @c f =<< (CB <$> applyM @c f b)
  applyM f (CD d) = appM @(AppSelector c (C x)) @c f =<< (CD <$> applyM @c f d)

  applySelective f pred val@(CB b) = appIf @c f pred val (CB (applySelective @c f pred b))
  applySelective f pred val@(CD d) = appIf @c f pred val (CD (applySelective @c f pred d))

  applySelectiveM f pred val@(CB b) = appIfM @c f pred val (CB <$> applySelectiveM @c f pred b)
  applySelectiveM f pred val@(CD d) = appIfM @c f pred val (CD <$> applySelectiveM @c f pred d)

-- instance (GoodOperationFor c (A x), GoodOperationFor c (A D), GoodOperationFor c B, GoodOperationFor c (C x), GoodOperationFor c D, GoodOperationFor c E) => AutoApply c False (C x) where
--   applyAuto f (CB b) = app @(AppSelector c (C x)) @c f $ CB (applyAuto @c @(ClassIgnoresSubtree c B) f b)
--   applyAuto f (CD d) = app @(AppSelector c (C x)) @c f $ CD (applyAuto @c @(ClassIgnoresSubtree c D) f d)

--   applyAutoM f (CB b) = appM @(AppSelector c (C x)) @c f =<< (CB <$> applyAutoM @c @(ClassIgnoresSubtree c B) f b)
--   applyAutoM f (CD d) = appM @(AppSelector c (C x)) @c f =<< (CD <$> applyAutoM @c @(ClassIgnoresSubtree c D) f d)

instance (GoodOperationFor c E, GoodOperationFor c D) => Apply c D where
  apply f (DDE d e) = app @(AppSelector c D) @c f $ DDE (apply @c f d) (apply @c f e)
  apply f D = app @(AppSelector c D) @c f $ D

  applyM f (DDE d e) = appM @(AppSelector c D) @c f =<< (DDE <$> applyM @c f d <*> applyM @c f e)
  applyM f D = appM @(AppSelector c D) @c f =<< (return D)

  applySelective f pred val@(DDE d e) = appIf @c f pred val (DDE (applySelective @c f pred d) (applySelective @c f pred e))
  applySelective f pred val@D = appIf @c f pred val D

  applySelectiveM f pred val@(DDE d e) = appIfM @c f pred val (DDE <$> applySelectiveM @c f pred d <*> applySelectiveM @c f pred e)
  applySelectiveM f pred val@D = appIfM @c f pred val (return D)

-- instance (GoodOperationFor c E, GoodOperationFor c D) => AutoApply c False D where
--   applyAuto f (DDE d e) = app @(AppSelector c D) @c f $ DDE (applyAuto @c @(ClassIgnoresSubtree c D) f d) (applyAuto @c @(ClassIgnoresSubtree c E) f e)
--   applyAuto f D = app @(AppSelector c D) @c f $ D

--   applyAutoM f (DDE d e) = appM @(AppSelector c D) @c f =<< (DDE <$> applyAutoM @c @(ClassIgnoresSubtree c D) f d <*> applyAutoM @c @(ClassIgnoresSubtree c E) f e)
--   applyAutoM f D = appM @(AppSelector c D) @c f =<< (return D)

instance (GoodOperationFor c E) => Apply c E where
  apply f E = app @(AppSelector c E) @c f E
  applyM f E = appM @(AppSelector c E) @c f E
  applySelective f pred val@E = appIf @c f pred val E
  applySelectiveM f pred val@E = appIfM @c f pred val (return E)

-- instance (GoodOperationFor c E) => AutoApply c False E where
--   applyAuto f E = app @(AppSelector c E) @c f E
--   applyAutoM f E = appM @(AppSelector c E) @c f E

