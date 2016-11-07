{-# LANGUAGE KindSignatures, TypeFamilies #-}
module MiniLanguage where


-- * Semantic info

data Dom
data NameInfo = NameInfo
data NoInfo = NoInfo


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
  data SpanInfo RangeStage = NodeSpan
  data ListInfo RangeStage = ListPos
  data OptionalInfo RangeStage = OptionalPos

-- * Nodes

data NodeInfo sema src 
  = NodeInfo { _semanticInfo :: sema
             , _sourceInfo :: src
             }

data Ann elem dom stage
  = Ann { _annotation :: NodeInfo (SemanticInfo dom elem) (SpanInfo stage)
        , _element    :: elem dom stage -- ^ The original AST part
        }


data Expr dom stage = Add (Ann Name dom stage) (Ann Name dom stage)
data Name dom stage = Name
