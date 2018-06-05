{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}

module Clapi.Serialisation.Path where

import Clapi.Serialisation.Base (Encodable(..), (<<>>))
import Clapi.Types.Path (Qualified(..), Seg, mkSeg, unSeg, Namespace(..))
import qualified Clapi.Types.Path as Path

instance Encodable Seg where
  builder = builder . unSeg
  parser = parser >>= mkSeg

instance Encodable (Path.Path a) where
  builder = builder . Path.toText
  parser = parser >>= Path.fromText

deriving instance Encodable Namespace

instance Encodable a => Encodable (Qualified a) where
  -- FIXME: we'll get two concatenated null terminated strings instead of ns:s
  -- for a TypeName
  builder (Qualified ns a) = builder ns <<>> builder a
  parser = Qualified <$> parser <*> parser
