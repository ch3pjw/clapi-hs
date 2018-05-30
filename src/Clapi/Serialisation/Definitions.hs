{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE QuasiQuotes #-}

module Clapi.Serialisation.Definitions where

import Clapi.Serialisation.Base
  (Encodable(..), (<<>>), tdTaggedBuilder, tdTaggedParser)
import Clapi.Serialisation.Path ()
import Clapi.TaggedData (TaggedData, taggedData)
import Clapi.TextSerialisation (ttToText, ttFromText)
import Clapi.TH (btq)
import Clapi.Types.Definitions
  ( Liberty(..), Required(..), MetaType(..), metaType
  , TupleDefinition(..), StructDefinition(..), ArrayDefinition(..)
  , Definition(..), defDispatch, PostDefinition(..))
import Clapi.Types.Tree (TreeType)

libertyTaggedData :: TaggedData Liberty Liberty
libertyTaggedData = taggedData toTag id
  where
    toTag l = case l of
      Cannot -> [btq|c|]
      May -> [btq|m|]
      Must -> [btq|M|]

instance Encodable Liberty where
  builder = tdTaggedBuilder libertyTaggedData $ const $ return mempty
  parser = tdTaggedParser libertyTaggedData return

requiredTaggedData :: TaggedData Required Required
requiredTaggedData = taggedData toTag id
  where
    toTag r = case r of
      Required -> [btq|r|]
      Optional -> [btq|o|]

instance Encodable Required where
  builder = tdTaggedBuilder requiredTaggedData $ const $ return mempty
  parser = tdTaggedParser requiredTaggedData return

-- FIXME: do we want to serialise the type to text first?!
instance Encodable TreeType where
  builder = builder . ttToText
  parser = parser >>= ttFromText

instance Encodable TupleDefinition where
  builder (TupleDefinition doc types interpl) =
    builder doc <<>> builder types <<>> builder interpl
  parser = TupleDefinition <$> parser <*> parser <*> parser

instance Encodable StructDefinition where
  builder (StructDefinition doc tyinfo) = builder doc <<>> builder tyinfo
  parser = StructDefinition <$> parser <*> parser

instance Encodable ArrayDefinition where
  builder (ArrayDefinition doc ct cl) =
    builder doc <<>> builder ct <<>> builder cl
  parser = ArrayDefinition <$> parser <*> parser <*> parser

defTaggedData :: TaggedData MetaType Definition
defTaggedData = taggedData typeToTag (defDispatch metaType)
  where
    typeToTag mt = case mt of
      Tuple -> [btq|T|]
      Struct -> [btq|S|]
      Array -> [btq|A|]

instance Encodable Definition where
  builder = tdTaggedBuilder defTaggedData $ \def -> case def of
    TupleDef d -> builder d
    StructDef d -> builder d
    ArrayDef d -> builder d
  parser = tdTaggedParser defTaggedData $ \mt -> case mt of
    Tuple -> TupleDef <$> parser
    Struct -> StructDef <$> parser
    Array -> ArrayDef <$> parser

instance Encodable PostDefinition where
  builder (PostDefinition doc args) = builder doc <<>> builder args
  parser = PostDefinition <$> parser <*> parser
