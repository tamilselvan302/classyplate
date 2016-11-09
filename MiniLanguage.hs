{-# LANGUAGE KindSignatures, TypeFamilies, DeriveGeneric #-}
module MiniLanguage where

import GHC.Generics

-- * Semantic info

data Dom deriving Generic
data NameInfo = NameInfo deriving Generic
data NoInfo = NoInfo deriving Generic


type SemanticInfo (domain :: *) (node :: * -> * -> *) = SemanticInfo' domain (SemaInfoClassify node)

data SameInfoNameCls
data SameInfoDefaultCls

type family SemaInfoClassify (node :: * -> * -> *) where
  SemaInfoClassify Name = SameInfoNameCls
  SemaInfoClassify a    = SameInfoDefaultCls

type family SemanticInfo' (domain :: *) (nodecls :: *)

type instance SemanticInfo' Dom SameInfoNameCls = NameInfo
type instance SemanticInfo' Dom SameInfoDefaultCls = NoInfo

-- * Source info

data RangeStage

class SourceInfo stage where
  data SpanInfo stage :: *
  data ListInfo stage :: *
  data OptionalInfo stage :: *

instance SourceInfo RangeStage where
  data SpanInfo RangeStage = NodeSpan deriving Generic
  data ListInfo RangeStage = ListPos deriving Generic
  data OptionalInfo RangeStage = OptionalPos deriving Generic

-- * Nodes

data NodeInfo sema src 
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             } deriving Generic

data Ann elem dom stage
  = Ann { _annotation :: NodeInfo (SemanticInfo dom elem) (SpanInfo stage)
        , _element    :: elem dom stage -- ^ The original AST part
        } deriving Generic

instance Show (elem dom stage) => Show (Ann elem dom stage) where
    show = show . _element


data Expr dom stage = Add (Ann Expr dom stage) (Ann Expr dom stage)
                    | Var (Ann Name dom stage)
   deriving (Show, Generic)

data Name dom stage = Name 
  deriving (Show, Generic)
