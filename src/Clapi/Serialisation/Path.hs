{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}

module Clapi.Serialisation.Path where

import Clapi.Serialisation.Base (Encodable(..), (<<>>))
import Clapi.Types.Path
  ( Qualified(..), Seg, mkSeg, unSeg, Namespace(..), AbsRel(..)
  , absPathFromText, relPathFromText)
import qualified Clapi.Types.Path as Path

instance Encodable Seg where
  builder = builder . unSeg
  parser = parser >>= mkSeg

instance Encodable (Path.Path 'Abs) where
  builder = builder . Path.toText
  parser = parser >>= absPathFromText

instance Encodable (Path.Path 'Rel) where
  builder = builder . Path.toText
  parser = parser >>= relPathFromText

deriving instance Encodable Namespace

instance Encodable a => Encodable (Qualified a) where
  -- FIXME: we'll get two concatenated null terminated strings instead of ns:s
  -- for a TypeName
  builder (Qualified ns a) = builder ns <<>> builder a
  parser = Qualified <$> parser <*> parser
