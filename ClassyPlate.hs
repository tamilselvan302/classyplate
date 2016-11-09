{-# LANGUAGE Rank2Types
           , ConstraintKinds
           , KindSignatures
           , TypeFamilies
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , AllowAmbiguousTypes
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , DataKinds
           , TypeApplications
           , DeriveGeneric
           , DeriveDataTypeable
           , TypeOperators
           , PolyKinds 
           #-}
module ClassyPlate where

import GHC.Exts
import Data.Maybe
import GHC.Generics (Generic)
import Data.Data (Data)
import Control.Parallel.Strategies

import TypePrune

-- FIXME: when TH supports type application we can remove the token parameters

type GoodOperationForAuto c e = (GoodOperationFor c e, Generic e)
type GoodOperationFor c e = (App (AppSelector c e) c e)

data ClsToken (c :: * -> Constraint)
data FlagToken (c :: Bool)

-- | A class for applying a function if the class of the functions allows the application
class App (flag :: Bool) c b where
  app :: FlagToken flag -> ClsToken c -> (forall a . c a => a -> a) -> b -> b
  appM :: Monad m => FlagToken flag -> ClsToken c -> (forall a . c a => a -> m a) -> b -> m b
  appOpt :: FlagToken flag -> ClsToken c -> (forall a . c a => a -> x) -> b -> Maybe x

instance c b => App 'True c b where
  {-# INLINE app #-}
  app _ _ f a = f a
  {-# INLINE appM #-}
  appM _ _ f a = f a
  {-# INLINE appOpt #-}
  appOpt _ _ f a = Just $ f a

instance App 'False c b where
  {-# INLINE app #-}
  app _ _ _ a = a
  {-# INLINE appM #-}
  appM _ _ _ a = return a
  {-# INLINE appOpt #-}
  appOpt _ _ _ _ = Nothing

class GoodOperationFor c b => ClassyPlate c b where
  apply :: ClsToken c -> (forall a . c a => a -> a) -> b -> b
  applyM :: Monad m => ClsToken c -> (forall a . c a => a -> m a) -> b -> m b

  applySelective :: ClsToken c -> (forall a . c a => a -> a) -> (forall a . c a => a -> Bool) -> b -> b
  applySelectiveM :: Monad m => ClsToken c -> (forall a . c a => a -> m a) -> (forall a . c a => a -> m Bool) -> b -> m b

class (GoodOperationForAuto c b) => AutoApply c (sel :: Bool) b where
  applyAuto :: FlagToken sel -> ClsToken c -> (forall a . c a => a -> a) -> b -> b
  applyAutoM :: Monad m => FlagToken sel -> ClsToken c -> (forall a . c a => a -> m a) -> b -> m b

applyAuto_ :: forall c b . AutoApply c (ClassIgnoresSubtree c b) b => (forall a . c a => a -> a) -> b -> b
{-# INLINE applyAuto_ #-}
applyAuto_ = applyAuto (undefined :: FlagToken (ClassIgnoresSubtree c b)) (undefined :: ClsToken c)

applyAutoM_ :: forall c b m . (AutoApply c (ClassIgnoresSubtree c b) b, Monad m) => (forall a . c a => a -> m a) -> b -> m b
{-# INLINE applyAutoM_ #-}
applyAutoM_ = applyAutoM (undefined :: FlagToken (ClassIgnoresSubtree c b)) (undefined :: ClsToken c)

instance (GoodOperationForAuto c b) => AutoApply c True b where
  applyAuto _ t f a = app (undefined :: FlagToken (AppSelector c b)) t f a
  {-# INLINE applyAuto #-}
  applyAutoM _ t f a = appM (undefined :: FlagToken (AppSelector c b)) t f a
  {-# INLINE applyAutoM #-}

{-# SPECIALIZE INLINE apply :: ClassyPlate (MonoMatch x) sel b => (forall a . MonoMatch x a => a -> a) -> b -> b #-}

class MonoMatch a b where
  monoApp :: (a -> a) -> b -> b

instance MonoMatch a a where
  monoApp = id
  {-# INLINE monoApp #-}

type instance AppSelector (MonoMatch a) b = TypEq a b

type family TypEq a b :: Bool where
  TypEq a a = 'True
  TypEq a b = 'False

appIf :: forall c b . App (AppSelector c b) c b => ClsToken c -> (forall a . c a => a -> a) -> (forall a . c a => a -> Bool) -> b -> b -> b
{-# INLINE appIf #-}
appIf t f pred val combined = app flTok t f $ case appOpt flTok t pred val of
    Just False -> val
    _          -> combined
  where flTok = undefined :: FlagToken (AppSelector c b)

appIfM :: forall c b m . (App (AppSelector c b) c b, Monad m) => ClsToken c -> (forall a . c a => a -> m a) -> (forall a . c a => a -> m Bool) -> b -> m b -> m b
{-# INLINE appIfM #-}
appIfM t f pred val combined = do
    doChildren <- fromMaybe (return True) $ appOpt flTok t pred val
    inner <- case doChildren of False -> return val
                                _     -> combined
    appM flTok t f inner
  where flTok = undefined :: FlagToken (AppSelector c b)