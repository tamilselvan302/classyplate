{-# LANGUAGE Rank2Types, ConstraintKinds, KindSignatures, TypeFamilies, ScopedTypeVariables, MultiParamTypeClasses, AllowAmbiguousTypes
           , FlexibleContexts, FlexibleInstances, UndecidableInstances, DataKinds, TypeApplications #-}
module Test where

import GHC.Exts


-------------------------------- USAGE-SPECIFIC PART

test = apply @F trf $ ABC (BA (ABC B C)) (CB B)

class F a where
  trf :: a -> a

instance F B where
  trf b = B

instance F C where
  trf c = C
  
type instance AppSelector F A = 'False
type instance AppSelector F B = 'True
type instance AppSelector F C = 'True


--------

test2 = applyM @Debug debugCs $ ABC (BA (ABC B C)) (CB B)

class Debug a where
  debugCs :: a -> IO a

instance Debug C where
  debugCs c = print c >> return c
  
type instance AppSelector Debug a = DebugSelector a

type family DebugSelector t where
  DebugSelector C = 'True
  DebugSelector _ = 'False

--------

test3 = apply @(MonoMatch C) (monoApp (\c -> case c of CB b -> CB B; C -> CB B)) $ ABC (BA (ABC B C)) (CB B)


-------------------------------- REPRESENTATION-SPECIFIC PART

data A = ABC B C deriving Show
data B = B | BA A deriving Show
data C = C | CB B deriving Show

type GoodOperation c = (App (AppSelector c A) c A, App (AppSelector c B) c B, App (AppSelector c C) c C)

instance GoodOperation c => Apply c A where
  apply f (ABC b c) = app @(AppSelector c A) @c f $ ABC (apply @c f b) (apply @c f c)
  applyM f (ABC b c) = appM @(AppSelector c A) @c f =<< (ABC <$> applyM @c f b <*> applyM @c f c)

instance GoodOperation c => Apply c B where
  apply f B = app @(AppSelector c B) @c f B
  apply f (BA a) = app @(AppSelector c B) @c f $ BA (apply @c f a)

  applyM f B = appM @(AppSelector c B) @c f B
  applyM f (BA a) = appM @(AppSelector c B) @c f =<< (BA <$> applyM @c f a)

instance GoodOperation c => Apply c C where
  apply f C = app @(AppSelector c C) @c f C
  apply f (CB b) = app @(AppSelector c C) @c f $ CB (apply @c f b)

  applyM f C = appM @(AppSelector c C) @c f C
  applyM f (CB b) = appM @(AppSelector c C) @c f =<< (CB <$> applyM @c f b)


--------------------------------- GENERIC PART


type family AppSelector (c :: * -> Constraint) (a :: *) :: Bool

class App (flag :: Bool) c b where
  app :: (forall a . c a => a -> a) -> b -> b
  appM :: Monad m => (forall a . c a => a -> m a) -> b -> m b

instance c b => App 'True c b where
  app f a = f a
  appM f a = f a

instance App 'False c b where
  app f a = a
  appM f a = return a

class App (AppSelector c b) c b => Apply c b where
  apply :: (forall a . c a => a -> a) -> b -> b
  applyM :: Monad m => (forall a . c a => a -> m a) -> b -> m b


class MonoMatch a b where
  monoApp :: (a -> a) -> b -> b

instance MonoMatch a a where
  monoApp = id

type instance AppSelector (MonoMatch a) b = TypEq a b

type family TypEq a b :: Bool where
  TypEq a a = 'True
  TypEq a b = 'False
