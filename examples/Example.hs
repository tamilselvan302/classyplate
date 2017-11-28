{-# LANGUAGE Rank2Types, ConstraintKinds, KindSignatures, TypeFamilies, ScopedTypeVariables, MultiParamTypeClasses, AllowAmbiguousTypes
           , FlexibleContexts, FlexibleInstances, UndecidableInstances, DataKinds, TypeApplications, DeriveGeneric, DeriveDataTypeable
           , TypeOperators, PolyKinds, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
module Example where

import GHC.Exts
import Data.Maybe
import GHC.Generics (Generic)
import qualified GHC.Generics as GG
import Data.Data (Data)
import Control.Monad
import Control.Parallel.Strategies

import Data.Type.Bool

import Data.Generics.ClassyPlate
import Data.Generics.ClassyPlate.Generate
import Debug.Trace


-------------------------------- USAGE-SPECIFIC PART

test :: A
test = bottomUp @F trf $ ABC (BA (ABC B (CB B))) (CB B)

class F a where
  trf :: a -> a

instance F B where
  trf b = b

instance F C where
  trf c = CD D

type instance AppSelector F A = 'False
type instance AppSelector F B = 'True
type instance AppSelector F C = 'True
type instance AppSelector F D = 'False
type instance AppSelector F E = 'False



topDownTrav :: forall c b . ClassyPlate c b => (forall a . (ClassyPlate c a, c a) => a -> a) -> b -> b
{-# INLINE topDownTrav #-}
topDownTrav trf = descend @c (topDownTrav @c trf . trf)

bottomUpTrav :: forall c b . ClassyPlate c b => (forall a . (ClassyPlate c a, c a) => a -> a) -> b -> b
{-# INLINE bottomUpTrav #-}
bottomUpTrav trf = descend @c (trf . bottomUpTrav @c trf)


--------

test2 = bottomUpM @Debug debugCs $ ABC (BA (ABC B (CB B))) (CD D)

class Debug a where
  debugCs :: a -> IO a

instance Debug C where
  debugCs c = print c >> return c
  
type instance AppSelector Debug a = DebugSelector a

type family DebugSelector t where
  DebugSelector C = 'True
  DebugSelector _ = 'False

--------

test3 :: A
test3 = bottomUp @(MonoMatch C) (monoApp (\c -> case c of CB b -> CB (BA (ABC b (CB B))); CD d -> CD D)) $ ABC (BA (ABC B (CB B))) (CB B)

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

type family DebugWhereSelector t where
  DebugWhereSelector C = 'True
  DebugWhereSelector D = 'True
  DebugWhereSelector E = 'True
  DebugWhereSelector _ = 'False

test4 = selectiveTraverseM @DebugWhere (\e -> (, debugSubtree e) <$> debugWhereCs e) $ ABC (BA (ABC B (CB B))) (CD (DDE D E))


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

