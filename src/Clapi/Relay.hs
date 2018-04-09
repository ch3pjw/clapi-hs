{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Clapi.Relay where

import Data.Either (isLeft, fromLeft, fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Void (Void)

import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.AssocList
  (alEmpty, alInsert, alFilterKey, unAssocList, alKeys, alMapKeys)
import Clapi.Types.Messages (ErrorIndex(..), namespaceErrIdx)
import Clapi.Types.Digests
  ( TrpDigest(..), TrprDigest(..)
  , FrpErrorDigest(..), DataChange(..)
  , TimeSeriesDataOp(..), DefOp(..)
  , OutboundDigest(..), InboundDigest(..)
  , OutboundClientDigest(..), OutboundClientInitialisationDigest
  , InboundClientDigest(..), OutboundProviderDigest(..)
  , DataDigest, ContainerOps)
import Clapi.Types.Path (Path, TypeName(..), pattern (:</), pattern Root)
import Clapi.Types.Definitions (Liberty, Definition(StructDef), StructDefinition(..))
import Clapi.Types.Wire (WireValue)
import Clapi.Types.SequenceOps (SequenceOp(..), presentAfter)
import Clapi.Tree (RoseTreeNode(..), TimeSeries)
import Clapi.Valuespace
  ( Valuespace(..), vsRelinquish, vsLookupDef
  , processToRelayProviderDigest, processToRelayClientDigest, valuespaceGet
  , getLiberty, rootTypeName)
import Clapi.Protocol (Protocol, waitThenFwdOnly, sendRev)

mapPartitionJust :: Map k (Maybe a) -> (Map k a, Set k)
mapPartitionJust m = let (js, ns) = Map.partition isJust m in
  (fromJust <$> js, Map.keysSet ns)

mapPartitionEither :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEither m = let (ls, rs) = Map.partition isLeft m in
  (fromLeft undefined <$> ls, fromRight undefined <$> rs)

oppifyTimeSeries :: TimeSeries [WireValue] -> DataChange
oppifyTimeSeries ts = TimeChange $
  Dkmap.flatten (\t (att, (i, wvs)) -> (att, OpSet t wvs i)) ts

genInitDigest
  :: Set Path -> Set TypeName -> Valuespace
  -> OutboundClientInitialisationDigest
genInitDigest ps tns vs =
  let
    rtns = Map.fromSet (flip valuespaceGet vs) ps
    (tnErrs, defs) = mapPartitionEither $ Map.fromSet (flip vsLookupDef vs) tns
    initialOcd = OutboundClientDigest mempty (OpDefine <$> defs) mempty alEmpty
      (pure . Text.pack <$> Map.mapKeys TypeError tnErrs)
  in
    Map.foldlWithKey go initialOcd rtns
  where
    go
      :: OutboundClientInitialisationDigest -> Path
      -> Either String (Definition, TypeName, Liberty, RoseTreeNode [WireValue])
      -> OutboundClientInitialisationDigest
    go d p (Left errStr) = d {
        ocdErrors = Map.unionWith (<>) (ocdErrors d)
          (Map.singleton (PathError p) [Text.pack errStr])
      }
    go d p (Right (def, tn, lib, rtn)) =
      let
        d' = d {
          ocdDefinitions = Map.insert tn (OpDefine def) (ocdDefinitions d),
          ocdTypeAssignments = Map.insert p (tn, lib) (ocdTypeAssignments d)}
      in case rtn of
        RtnEmpty -> error "Valid tree should not contain empty nodes, but did"
        RtnChildren kidsAl ->
          let (kidSegs, kidAtts) = unzip $ unAssocList kidsAl in
          d' {
            ocdContainerOps = Map.insert p
              (Map.fromList $ zipWith3
                (\s a att -> (s, (att, SoPresentAfter a)))
                kidSegs (Nothing : (Just <$> kidSegs)) kidAtts)
            (ocdContainerOps d')
          }
        RtnConstData att vals -> d'{ocdData =
          alInsert p (ConstChange att vals) $ ocdData d}
        RtnDataSeries ts ->
          d'{ocdData = alInsert p (oppifyTimeSeries ts) $ ocdData d}

relay
  :: Monad m => Valuespace
  -> Protocol (i, InboundDigest) Void (i, OutboundDigest) Void m ()
relay vs = waitThenFwdOnly fwd
  where
    fwd (i, dig) = case dig of
        Ipd d -> either
          terminalErrors
          (handleOwnerSuccess d) $ processToRelayProviderDigest d vs
        Icd d -> handleClientDigest d
          $ processToRelayClientDigest (icdContainerOps d) (icdData d) vs
        Iprd (TrprDigest ns) -> do
          let vs' = vsRelinquish ns vs
          sendRev (i, Ocd $ OutboundClientDigest
            -- FIXME: Attributing revocation to nobody!
            (Map.singleton Root $ Map.singleton ns (Nothing, SoAbsent))
            (Map.insert rootTypeName
              (OpDefine $ fromJust $ vsLookupDef rootTypeName vs') $
              (fmap (const OpUndefine) $ Map.mapKeys (TypeName ns) $
                 Map.findWithDefault mempty ns $ vsTyDefs vs))
            mempty alEmpty mempty)
          relay vs'
      where
        handleOwnerSuccess
            (TrpDigest ns defs dd contOps errs) (updatedTyAssns, vs') =
          let
            shouldPubRoot =
              Map.member ns defs &&
              Map.notMember ns (vsTyDefs vs)
            rootDef = fromJust $ vsLookupDef rootTypeName vs'
            nsContOp (StructDef (StructDefinition _ kids)) = Map.singleton ns
              (Nothing, SoPresentAfter $ presentAfter ns $ alKeys kids)
            nsContOp _ = error "Root def not a struct WTAF"
            qDd = maybe (error "Bad sneakers") id $ alMapKeys (ns :</) dd
            qDd' = vsMinimiseDataDigest qDd vs
            errs' = Map.mapKeys (namespaceErrIdx ns) errs
            qDefs = Map.mapKeys (TypeName ns) defs
            qDefs' = vsMinimiseDefinitions qDefs vs
            qDefs'' = if shouldPubRoot
              then Map.insert rootTypeName (OpDefine rootDef) qDefs'
              else qDefs'
            qContOps = Map.mapKeys (ns :</) contOps
            qContOps' = vsMinimiseContOps qContOps vs
            qContOps'' = if shouldPubRoot
              then Map.insert Root (nsContOp rootDef) qContOps'
              else qContOps'
            mungedTas = Map.mapWithKey
              (\p tn -> (tn, either error id $ getLiberty p vs')) updatedTyAssns
          in do
            sendRev (i,
              Ocd $ OutboundClientDigest
                qContOps''
                -- FIXME: we need to provide defs for type assignments too.
                qDefs''
                mungedTas qDd' errs')
            relay vs'
        handleClientDigest
            (InboundClientDigest gets typeGets contOps dd) errMap =
          let
            -- TODO: Be more specific in what we reject (filtering by TpId
            -- rather than entire path)
            eidxPath eidx = case eidx of
                PathError p -> Just p
                TimePointError p _ -> Just p
                _ -> Nothing
            errPaths = Set.fromList $ mapMaybe eidxPath $ Map.keys errMap
            dd' = alFilterKey (\k -> not $ Set.member k errPaths) dd
            contOps' = Map.filterWithKey
              (\k _ -> not $ Set.member k errPaths) contOps
            dd'' = vsMinimiseDataDigest dd' vs
            contOps'' = vsMinimiseContOps contOps' vs
            -- FIXME: above uses errors semantically and shouldn't (thus throws
            -- away valid time point changes)
            cid = genInitDigest gets typeGets vs
            cid' = cid{ocdErrors =
              Map.unionWith (<>) (ocdErrors cid) (fmap (Text.pack . show) <$> errMap)}
          in do
            sendRev (i, Ocid $ cid')
            sendRev (i, Opd $ OutboundProviderDigest contOps'' dd'')
            relay vs
        terminalErrors errMap = do
          sendRev (i, Ope $ FrpErrorDigest errMap)
          relay vs

-- FIXME: Worst case implementation
vsMinimiseDefinitions :: Map TypeName DefOp -> Valuespace -> Map TypeName DefOp
vsMinimiseDefinitions defs _ = defs

-- FIXME: Worst case implementation
vsMinimiseDataDigest :: DataDigest -> Valuespace -> DataDigest
vsMinimiseDataDigest dd _ = dd

-- FIXME: Worst case implementation
vsMinimiseContOps :: ContainerOps -> Valuespace -> ContainerOps
vsMinimiseContOps contOps _ = contOps
