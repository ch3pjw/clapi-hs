{-# LANGUAGE
    DataKinds
  , TypeFamilies
#-}
module Clapi.Types.Messages where

import Data.Bifunctor (bimap)
import Data.Map (Map)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word (Word32)

import Clapi.Types.Base (Attributee, Time, Interpolation)
import Clapi.Types.Definitions (Definition, Editable, PostDefinition)
import Clapi.Types.Path
  ( Seg, Path, AbsRel(..), TypeName, qualify, unqualify, Namespace(..)
  , mkAbsPath)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Wire (WireValue)

-- FIXME: redefinition
type TpId = Word32

type family EiAbsRel a :: AbsRel
type instance EiAbsRel Seg = 'Rel
type instance EiAbsRel TypeName = 'Abs

data ErrorIndex a
  = GlobalError
  | PathError (Path (EiAbsRel a))
  | TimePointError (Path (EiAbsRel a)) TpId
  | PostTypeError (Tagged PostDefinition a)
  | TypeError (Tagged Definition a)
  deriving (Show, Eq, Ord)

splitErrIdx :: ErrorIndex TypeName -> Maybe (Namespace, ErrorIndex Seg)
splitErrIdx ei = case ei of
  GlobalError -> Nothing
  PathError p -> bimap Namespace PathError <$> Path.splitHead p
  TimePointError p tpid -> bimap Namespace (flip TimePointError tpid) <$>
    Path.splitHead p
  PostTypeError tn -> Just $ fmap PostTypeError $ unqualify tn
  TypeError tn -> Just $ fmap TypeError $ unqualify tn

namespaceErrIdx :: Namespace -> ErrorIndex Seg -> ErrorIndex TypeName
namespaceErrIdx ns ei = case ei of
  GlobalError -> GlobalError
  PathError p -> PathError $ mkAbsPath ns p
  TimePointError p tpid -> TimePointError (mkAbsPath ns p) tpid
  PostTypeError s -> PostTypeError $ qualify ns s
  TypeError s -> TypeError $ qualify ns s

data MsgError a
  = MsgError {errIndex :: ErrorIndex a, errMsgTxt :: Text} deriving (Eq, Show)

data DefMessage ident def
  = MsgDefine ident def
  | MsgUndefine ident
  deriving (Show, Eq)

-- FIXME: might be nicer to break this up into sub and unsub values typed by
-- what they are subscriptions for:
data SubMessage
  = MsgSubscribe {subMsgPath :: Path 'Abs}
  | MsgPostTypeSubscribe {subMsgPostTypeName :: Tagged PostDefinition TypeName}
  | MsgTypeSubscribe {subMsgTypeName :: Tagged Definition TypeName}
  | MsgUnsubscribe {subMsgPath :: Path 'Abs}
  | MsgPostTypeUnsubscribe
    {subMsgPostTypeName :: Tagged PostDefinition TypeName}
  | MsgTypeUnsubscribe {subMsgTypeName :: Tagged Definition TypeName}
  deriving (Eq, Show)

data TypeMessage
  = MsgAssignType (Path 'Abs) (Tagged Definition TypeName) Editable
  deriving (Show, Eq)

data PostMessage (ar :: AbsRel)
  = MsgPost
  { pMsgPath :: Path ar
  , pMsgPlaceholder :: Seg
  , pMsgArgs :: Map Seg WireValue
  } deriving (Show, Eq)

data DataUpdateMessage (ar :: AbsRel)
  = MsgConstSet
      { duMsgPath :: Path ar
      , duMsgArgs :: [WireValue]
      , duMsgAttributee :: Maybe Attributee
      }
  | MsgSet
      { duMsgPath :: Path ar
      , duMsgTpId :: TpId
      , duMsgTime :: Time
      , duMsgArgs :: [WireValue]
      , duMsgInterpolation :: Interpolation
      , duMsgAttributee :: Maybe Attributee
      }
  | MsgRemove
      { duMsgPath :: Path ar
      , duMsgTpId :: Word32
      , duMsgAttributee :: Maybe Attributee
      }
   deriving (Eq, Show)

data ContainerUpdateMessage (ar :: AbsRel)
  = MsgMoveAfter
      { cuMsgPath :: Path ar
      , cuMsgTarg :: Seg
      , cuMsgRef :: Maybe Seg
      , cuMsgAttributee :: Maybe Attributee
      }
  deriving (Eq, Show)

data DeleteMessage (ar :: AbsRel)
  = MsgDelete
      { dMsgPath :: Path ar
      , dMsgAttributee :: Maybe Attributee
      }
  deriving (Eq, Show)

data ToRelayProviderBundle = ToRelayProviderBundle
  { trpbNamespace :: Namespace
  , trpbErrors :: [MsgError Seg]
  , trpbPostDefs :: [DefMessage (Tagged PostDefinition Seg) PostDefinition]
  , trpbDefinitions :: [DefMessage (Tagged Definition Seg) Definition]
  , trpbDeletes :: [DeleteMessage 'Rel]
  , trpbData :: [DataUpdateMessage 'Rel]
  , trpbContMsgs :: [ContainerUpdateMessage 'Rel]
  } deriving (Show, Eq)

data ToRelayProviderRelinquish
  = ToRelayProviderRelinquish Namespace deriving (Show, Eq)

data FromRelayProviderBundle = FromRelayProviderBundle
  { frpbNamespace :: Namespace
  , frpbDeletes :: [DeleteMessage 'Rel]
  , frpbPosts :: [PostMessage 'Rel]
  , frpbData :: [DataUpdateMessage 'Rel]
  , frpbContMsgs :: [ContainerUpdateMessage 'Rel]
  } deriving (Show, Eq)

data FromRelayProviderErrorBundle = FromRelayProviderErrorBundle
  { frpebErrors :: [MsgError TypeName]
  } deriving (Eq, Show)

data ToRelayClientBundle = ToRelayClientBundle
  { trcbSubs :: [SubMessage]
  , trcbDeletes :: [DeleteMessage 'Abs]
  , trcbPosts :: [PostMessage 'Abs]
  , trcbData :: [DataUpdateMessage 'Abs]
  , trcbContMsgs :: [ContainerUpdateMessage 'Abs]
  } deriving (Eq, Show)

data FromRelayClientBundle = FromRelayClientBundle
  { frcbPostTypeUnsubs :: [Tagged PostDefinition TypeName]
  , frcbTypeUnsubs :: [Tagged Definition TypeName]
  , frcbDataUnsubs :: [Path 'Abs]
  , frcbErrors :: [MsgError TypeName]
  , frcbPostDefs :: [DefMessage (Tagged PostDefinition TypeName) PostDefinition]
  , frcbDefinitions :: [DefMessage (Tagged Definition TypeName) Definition]
  , frcbTypeAssignments :: [TypeMessage]
  , frcbDeletes :: [DeleteMessage 'Abs]
  , frcbData :: [DataUpdateMessage 'Abs]
  , frcbContMsgs :: [ContainerUpdateMessage 'Abs]
  } deriving (Show, Eq)

data ToRelayBundle
  = Trpb ToRelayProviderBundle
  | Trpr ToRelayProviderRelinquish
  | Trcb ToRelayClientBundle
  deriving (Show, Eq)

data FromRelayBundle
  = Frpb FromRelayProviderBundle
  | Frpeb FromRelayProviderErrorBundle
  | Frcb FromRelayClientBundle
  deriving (Show, Eq)
