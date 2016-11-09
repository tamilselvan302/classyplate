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
