{-# LANGUAGE FlexibleContexts #-}

module Clapi.Relay where

import Control.Monad (unless)
import Data.Either (isLeft, fromLeft, fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Strict.Merge (merge, preserveMissing, mapMissing, zipWithMaybeMatched)
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Monoid
import Data.Set (Set)
import Data.Tagged (Tagged(..))
import qualified Data.Text as Text
import Data.Void (Void)

import Clapi.Types.Base (Attributee)
import qualified Clapi.Types.Dkmap as Dkmap
import Clapi.Types.AssocList
  (AssocList, alEmpty, alInsert, alFilterKey, unAssocList, alMapKeys, alFoldlWithKey)
import Clapi.Types.Messages (ErrorIndex(..), namespaceErrIdx)
import Clapi.Types.Digests
  ( TrpDigest(..), TrprDigest(..)
  , FrpErrorDigest(..), DataChange(..)
  , TimeSeriesDataOp(..), DefOp(..)
  , OutboundDigest(..), InboundDigest(..)
  , OutboundClientDigest(..), OutboundClientInitialisationDigest, ocdNull, opdNull
  , InboundClientDigest(..), OutboundProviderDigest(..)
  , DataDigest, ContainerOps)
import Clapi.Types.Path
  ( Seg, Path, TypeName, qualify,  pattern (:</), pattern Root, pattern (:/)
  , parentPath , Namespace(..))
import Clapi.Types.Definitions (Editable, Definition, PostDefinition)
import Clapi.Types.Wire (WireValue)
import Clapi.Tree (RoseTreeNode(..), TimeSeries, treeLookupNode)
import Clapi.Valuespace
  ( Valuespace(..), vsRelinquish, vsLookupPostDef, vsLookupDef
  , processToRelayProviderDigest, processToRelayClientDigest, valuespaceGet
  , getEditable, rootTypeName)
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
  :: Set Path -> Set (Tagged PostDefinition TypeName)
  -> Set (Tagged Definition TypeName) -> Valuespace
  -> OutboundClientInitialisationDigest
genInitDigest ps ptns tns vs =
  let
    rtns = Map.fromSet (flip valuespaceGet vs) ps
    (ptnErrs, postDefs) = mapPartitionEither $
        Map.fromSet (flip vsLookupPostDef vs) ptns
    (tnErrs, defs) = mapPartitionEither $ Map.fromSet (flip vsLookupDef vs) tns
    errMap = (pure . Text.pack <$> Map.mapKeys TypeError tnErrs) <>
      (pure . Text.pack <$> Map.mapKeys PostTypeError ptnErrs)
    initialOcd = OutboundClientDigest mempty
      (OpDefine <$> postDefs) (OpDefine <$> defs) mempty mempty alEmpty errMap
  in
    Map.foldlWithKey go initialOcd rtns
  where
    go
      :: OutboundClientInitialisationDigest -> Path
      -> Either
          String
          ( Definition
          , Tagged Definition TypeName
          , Editable
          , RoseTreeNode [WireValue])
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
        RtnChildren kidsAl -> d' {ocdContainerOps =
           Map.insert p (childAfters kidsAl) $ ocdContainerOps d'}
        RtnConstData att vals -> d'{ocdData =
          alInsert p (ConstChange att vals) $ ocdData d}
        RtnDataSeries ts ->
          d'{ocdData = alInsert p (oppifyTimeSeries ts) $ ocdData d}

-- | Compares two lists of child keys returning the keys that are only present
--   in the first (i.e. removals), the dependencies for keys that are in both
--   (changes) and the dependencies for keys that are only in the second
--   (additions).
contDiff
  :: [Seg] -> [Seg] -> (Set Seg, Map Seg (Maybe Seg), Map Seg (Maybe Seg))
contDiff a b =
    ( Set.difference (Map.keysSet $ aftersFor a) (Map.keysSet $ aftersFor b)
    , Map.intersection (aftersFor b) (aftersFor a)
    , Map.difference (aftersFor b) (aftersFor a))
  where
    aftersFor :: [Seg] -> Map Seg (Maybe Seg)
    aftersFor segs = Map.fromList $ zip segs (Nothing : (Just <$> segs))

childAfters
  :: AssocList Seg (Maybe Attributee) -> Map Seg (Maybe Attributee, Maybe Seg)
childAfters children = let (segs, atts) = unzip $ unAssocList children in
  Map.fromList $ zipWith3 (\s after att -> (s, (att, after)))
    segs (Nothing : (Just <$> segs)) atts

-- mayContDiff
--   :: Maybe (RoseTreeNode a) -> AssocList Seg (Maybe Attributee)
--   -> Maybe (Map Seg (Maybe Attributee, SequenceOp Seg))
-- mayContDiff ma kb = case ma of
--     Just (RtnChildren ka) -> if ka == kb
--         then Nothing
--         else Just $ contDiff ka kb
--     _ -> Nothing

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
            mempty
            (fmap (const OpUndefine) $ Map.mapKeys (qualify ns) $
               Map.findWithDefault mempty ns $ vsPostDefs vs)
            (Map.insert rootTypeName
              (OpDefine $ fromJust $ vsLookupDef rootTypeName vs') $
              (fmap (const OpUndefine) $ Map.mapKeys (qualify ns) $
                 Map.findWithDefault mempty ns $ vsTyDefs vs))
            mempty
            -- FIXME: Attributing revocation to nobody!
            (Map.singleton (Root :/ unNamespace ns) Nothing)
            alEmpty mempty)
          relay vs'
      where
        handleOwnerSuccess
            (TrpDigest ns postDefs defs dels dd contOps errs)
            (updatedTyAssns, vs') =
          let
            shouldPubRoot =
              Map.member (Tagged $ unNamespace ns) defs &&
              Map.notMember ns (vsTyDefs vs)
            rootDef = fromJust $ vsLookupDef rootTypeName vs'
            qDd = maybe (error "Bad sneakers") id $
              alMapKeys (unNamespace ns :</) dd
            qDd' = vsMinimiseDataDigest qDd vs
            errs' = Map.mapKeys (namespaceErrIdx ns) errs
            qPostDefs = Map.mapKeys (qualify ns) postDefs
            qDefs = Map.mapKeys (qualify ns) defs
            qDefs' = vsMinimiseDefinitions qDefs vs
            qDefs'' = if shouldPubRoot
              then Map.insert rootTypeName (OpDefine rootDef) qDefs'
              else qDefs'
            qContOps = Map.mapKeys (unNamespace ns :</) contOps
            qContOps' = vsMinimiseContOps qContOps vs
            mungedTas = Map.mapWithKey
              (\p tn ->
                 (tn, either error id $ getEditable p vs')) updatedTyAssns
            -- getContOps p = case fromJust $ treeLookupNode p $ vsTree vs' of
            --   RtnChildren kb -> (p,) <$> mayContDiff (treeLookupNode p $ vsTree vs) kb
            --   _ -> Nothing
            -- extraCops = Map.fromAscList $ mapMaybe (\p -> parentPath p >>= getContOps) $
            --   Set.toAscList $ Map.keysSet updatedTyAssns
            -- qContOps'' = extraCops <> qContOps'
          in do
            sendRev (i,
              Ocd $ OutboundClientDigest
                qContOps'
                qPostDefs
                -- FIXME: we need to provide defs for type assignments too.
                qDefs''
                mungedTas
                dels -- plus something from the getContOps stuff above I reckon
                qDd' errs')
            relay vs'
        handleClientDigest
            (InboundClientDigest gets postTypeGets typeGets contOps dels dd)
            errMap =
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
            cid = genInitDigest gets postTypeGets typeGets vs
            cid' = cid{ocdErrors =
              Map.unionWith (<>) (ocdErrors cid) (fmap (Text.pack . show) <$> errMap)}
            opd = OutboundProviderDigest dels contOps'' dd''
          in do
            unless (ocdNull cid') $ sendRev (i, Ocid cid')
            unless (opdNull opd) $ sendRev (i, Opd opd)
            relay vs
        terminalErrors errMap = do
          sendRev (i, Ope $ FrpErrorDigest errMap)
          relay vs

-- FIXME: Worst case implementation
vsMinimiseDefinitions
  :: Map (Tagged def TypeName) (DefOp def) -> Valuespace
  -> Map (Tagged def TypeName) (DefOp def)
vsMinimiseDefinitions defs _ = defs

-- FIXME: Worst case implementation
vsMinimiseDataDigest :: DataDigest -> Valuespace -> DataDigest
vsMinimiseDataDigest dd _ = dd

-- FIXME: Worst case implementation
vsMinimiseContOps :: ContainerOps -> Valuespace -> ContainerOps
vsMinimiseContOps contOps _ = contOps
