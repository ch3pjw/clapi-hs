{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleContexts
  , FlexibleInstances
  , UndecidableInstances
#-}

module Clapi.Serialisation.Messages where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))

import Blaze.ByteString.Builder (Builder)

import Data.Attoparsec.ByteString (Parser)

import Clapi.Serialisation.Base
  (Encodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Definitions ()
import Clapi.Serialisation.Path ()
import Clapi.TH (btq)
import Clapi.Types.Messages
import Clapi.Types.Path (Path)
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.Serialisation.Wire ()

data ErrIdxType
  = EitGlobal
  | EitPath
  | EitTimePoint
  | EitPostTypeName
  | EitTypeName
  deriving (Enum, Bounded)

errIdxTaggedData :: TaggedData ErrIdxType (ErrorIndex a)
errIdxTaggedData = taggedData typeToTag eiToType
  where
    typeToTag ty = case ty of
      EitGlobal -> [btq|g|]
      EitPath -> [btq|p|]
      EitTimePoint -> [btq|t|]
      EitPostTypeName -> [btq|c|]  -- "create"
      EitTypeName -> [btq|n|]
    eiToType ei = case ei of
      GlobalError -> EitGlobal
      PathError _ -> EitPath
      TimePointError _ _ -> EitTimePoint
      PostTypeError _ -> EitPostTypeName
      TypeError _ -> EitTypeName

instance (Encodable a, Encodable (Path (EiAbsRel a)))
    => Encodable (ErrorIndex a) where
  builder = tdTaggedBuilder errIdxTaggedData $ \ei -> case ei of
    GlobalError -> return mempty
    PathError p -> builder p
    TimePointError p tpid -> builder p <<>> builder tpid
    PostTypeError tn -> builder tn
    TypeError tn -> builder tn
  parser = tdTaggedParser errIdxTaggedData $ \eit -> case eit of
    EitGlobal -> return GlobalError
    EitPath -> PathError <$> parser
    EitTimePoint -> TimePointError <$> parser <*> parser
    EitPostTypeName -> PostTypeError <$> parser
    EitTypeName -> TypeError <$> parser


instance (Encodable a, Encodable (Path (EiAbsRel a)))
    => Encodable (MsgError a) where
  builder (MsgError ei s) = builder ei <<>> builder s
  parser = MsgError <$> parser <*> parser

data DefMsgType = DefMsgTDef | DefMsgTUndef deriving (Enum, Bounded)

defMsgTaggedData :: TaggedData DefMsgType (DefMessage a def)
defMsgTaggedData = taggedData typeToTag msgToType
  where
    typeToTag ty = case ty of
      DefMsgTDef -> [btq|d|]
      DefMsgTUndef -> [btq|u|]
    msgToType msg = case msg of
      MsgDefine _ _ -> DefMsgTDef
      MsgUndefine _ -> DefMsgTUndef

instance (Encodable ident, Encodable def)
  => Encodable (DefMessage ident def) where
    builder = tdTaggedBuilder defMsgTaggedData $ \msg -> case msg of
      MsgDefine tn def -> builder tn <<>> builder def
      MsgUndefine tn -> builder tn
    parser = tdTaggedParser defMsgTaggedData $ \defTy -> case defTy of
      DefMsgTDef -> MsgDefine <$> parser <*> parser
      DefMsgTUndef -> MsgUndefine <$> parser

data SubMsgType
  = SubMsgTSub
  | SubMsgTPostTypeSub
  | SubMsgTTypeSub
  | SubMsgTUnsub
  | SubMsgTPostTypeUnsub
  | SubMsgTTypeUnsub
  deriving (Enum, Bounded)

subMsgTaggedData :: TaggedData SubMsgType SubMessage
subMsgTaggedData = taggedData typeToTag msgToType
  where
    typeToTag (SubMsgTSub) = [btq|S|]
    typeToTag (SubMsgTPostTypeSub) = [btq|P|]
    typeToTag (SubMsgTTypeSub) = [btq|T|]
    typeToTag (SubMsgTUnsub) = [btq|s|]
    typeToTag (SubMsgTPostTypeUnsub) = [btq|p|]
    typeToTag (SubMsgTTypeUnsub) = [btq|t|]
    msgToType (MsgSubscribe _) = SubMsgTSub
    msgToType (MsgPostTypeSubscribe _) = SubMsgTPostTypeSub
    msgToType (MsgTypeSubscribe _) = SubMsgTTypeSub
    msgToType (MsgUnsubscribe _) = SubMsgTUnsub
    msgToType (MsgPostTypeUnsubscribe _) = SubMsgTPostTypeUnsub
    msgToType (MsgTypeUnsubscribe _) = SubMsgTTypeUnsub

instance Encodable SubMessage where
    builder = tdTaggedBuilder subMsgTaggedData $ \m -> case m of
        MsgSubscribe p -> builder p
        MsgUnsubscribe p -> builder p
        MsgTypeSubscribe t -> builder t
        MsgTypeUnsubscribe t -> builder t
        MsgPostTypeSubscribe t -> builder t
        MsgPostTypeUnsubscribe t -> builder t
    parser = tdTaggedParser subMsgTaggedData $ \e -> case e of
        (SubMsgTSub) -> MsgSubscribe <$> parser
        (SubMsgTPostTypeSub) -> MsgPostTypeSubscribe <$> parser
        (SubMsgTTypeSub) -> MsgTypeSubscribe <$> parser
        (SubMsgTUnsub) -> MsgUnsubscribe <$> parser
        (SubMsgTPostTypeUnsub) -> MsgPostTypeUnsubscribe <$> parser
        (SubMsgTTypeUnsub) -> MsgTypeUnsubscribe <$> parser

instance Encodable TypeMessage where
    builder (MsgAssignType p tn l) = builder p <<>> builder tn <<>> builder l
    parser = MsgAssignType <$> parser <*> parser <*> parser

instance Encodable (Path ar) => Encodable (PostMessage ar) where
    builder (MsgPost p ph args) = builder p <<>> builder ph <<>> builder args
    parser = MsgPost <$> parser <*> parser <*> parser

instance Encodable (Path ar) => Encodable (DeleteMessage ar) where
    builder (MsgDelete p att) = builder p <<>> builder att
    parser = MsgDelete <$> parser <*> parser

data DataUpdateMsgType
  = DUMTConstSet
  | DUMTSet
  | DUMTRemove
  deriving (Enum, Bounded)

dumtTaggedData :: TaggedData DataUpdateMsgType (DataUpdateMessage ar)
dumtTaggedData = taggedData typeToTag msgToType
  where
    typeToTag DUMTConstSet = [btq|S|]
    typeToTag DUMTSet = [btq|s|]
    typeToTag DUMTRemove = [btq|r|]
    msgToType (MsgConstSet {}) = DUMTConstSet
    msgToType (MsgSet {}) = DUMTSet
    msgToType (MsgRemove {}) = DUMTRemove

dumtParser
  :: Encodable (Path ar) => DataUpdateMsgType -> Parser (DataUpdateMessage ar)
dumtParser e = case e of
    DUMTConstSet -> MsgConstSet <$> parser <*> parser <*> parser
    DUMTSet -> MsgSet <$> parser <*> parser <*> parser <*> parser <*> parser
      <*> parser
    DUMTRemove -> MsgRemove <$> parser <*> parser <*> parser

dumtBuilder
  :: (Encodable (Path ar), MonadFail m) => DataUpdateMessage ar -> m Builder
dumtBuilder m = case m of
    MsgConstSet p v a -> builder p <<>> builder v <<>> builder a
    MsgSet p ptId t v i a ->
      builder p <<>> builder ptId <<>> builder t <<>> builder v <<>> builder i
      <<>> builder a
    MsgRemove p t a ->
      builder p <<>> builder t <<>> builder a

instance Encodable (Path ar) => Encodable (DataUpdateMessage ar) where
    builder = tdTaggedBuilder dumtTaggedData dumtBuilder
    parser = tdTaggedParser dumtTaggedData dumtParser

instance Encodable (Path ar) => Encodable (ContainerUpdateMessage ar) where
    parser =  MsgMoveAfter <$> parser <*> parser <*> parser <*> parser
    builder (MsgMoveAfter p s targ att) =
      builder p <<>> builder s <<>> builder targ <<>> builder att

data TrBundleType
  = TrbtProvider | TrbtProviderRelinquish | TrbtClient deriving (Enum, Bounded)

trBundleTaggedData :: TaggedData TrBundleType ToRelayBundle
trBundleTaggedData = taggedData typeToTag bundleToType
  where
    typeToTag ty = case ty of
      TrbtProvider -> [btq|P|]
      TrbtProviderRelinquish -> [btq|R|]
      TrbtClient ->  [btq|C|]
    bundleToType bund = case bund of
      Trpb _ -> TrbtProvider
      Trpr _ -> TrbtProviderRelinquish
      Trcb _ -> TrbtClient

instance Encodable ToRelayBundle where
    builder = tdTaggedBuilder trBundleTaggedData $ \bund -> case bund of
      Trpb (ToRelayProviderBundle ns errs postDefs defs dels dat contMsgs) ->
        builder ns <<>> builder errs <<>> builder postDefs <<>> builder defs
        <<>> builder dels <<>> builder dat <<>> builder contMsgs
      Trpr (ToRelayProviderRelinquish ns) -> builder ns
      Trcb (ToRelayClientBundle subs dels posts dat contMsgs) ->
        builder subs <<>> builder dels <<>> builder posts <<>> builder dat
        <<>> builder contMsgs
    parser = tdTaggedParser trBundleTaggedData $ \ty -> case ty of
      TrbtProvider -> Trpb <$>
        (ToRelayProviderBundle <$> parser <*> parser <*> parser <*> parser
        <*> parser <*> parser <*> parser)
      TrbtProviderRelinquish -> Trpr . ToRelayProviderRelinquish <$> parser
      TrbtClient -> Trcb <$>
        (ToRelayClientBundle <$> parser <*> parser <*> parser <*> parser
         <*> parser)

data FrBundleType
  = FrbtProvider | FrbtProviderError | FrbtClient deriving (Enum, Bounded)

frBundleTaggedData :: TaggedData FrBundleType FromRelayBundle
frBundleTaggedData = taggedData typeToTag bundleToType
  where
    typeToTag ty = case ty of
      FrbtProvider -> [btq|P|]
      FrbtProviderError -> [btq|E|]
      FrbtClient ->  [btq|C|]
    bundleToType bund = case bund of
      Frpb _ -> FrbtProvider
      Frpeb _ -> FrbtProviderError
      Frcb _ -> FrbtClient

instance Encodable FromRelayBundle where
    builder = tdTaggedBuilder frBundleTaggedData $ \bund -> case bund of
      Frpb (FromRelayProviderBundle ns dels posts dat contMsgs) ->
        builder ns <<>> builder dels <<>> builder posts <<>> builder dat
        <<>> builder contMsgs
      Frpeb (FromRelayProviderErrorBundle errs) -> builder errs
      Frcb (
          FromRelayClientBundle pTyUns tyUns datUns errs postDefs defs tas dels
          dat contMsgs) ->
        builder pTyUns <<>> builder tyUns <<>> builder datUns
        <<>> builder errs <<>> builder postDefs <<>> builder defs
        <<>> builder tas <<>> builder dels <<>> builder dat
        <<>> builder contMsgs
    parser = tdTaggedParser frBundleTaggedData $ \ty -> case ty of
      FrbtProvider -> Frpb <$>
        (FromRelayProviderBundle <$> parser <*> parser <*> parser <*> parser
         <*> parser)
      FrbtProviderError ->
        Frpeb . FromRelayProviderErrorBundle <$> parser
      FrbtClient -> Frcb <$>
        (FromRelayClientBundle <$> parser <*> parser <*>  parser <*> parser
        <*> parser <*> parser <*> parser <*> parser <*> parser <*> parser)
