
module Clapi.RelayApi (relayApiProto, PathSegable(..)) where

import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text

import Clapi.PerClientProto (ClientEvent(..), ServerEvent(..))
import Clapi.Types
  ( TrDigest(..), TrpDigest(..), FrDigest(..), FrpDigest(..), WireValue(..)
  , TimeStamped(..), Editable(..))
import Clapi.Types.AssocList (alSingleton, alFromMap, alFmapWithKey, alFromList)
import Clapi.Types.Base (InterpolationLimit(ILUninterpolated), Attributee)
import Clapi.Types.Definitions (tupleDef, structDef, arrayDef)
import Clapi.Types.Digests
  (DefOp(OpDefine), DataChange(..), FrcDigest(..), DataDigest)
import Clapi.Types.Path
  ( Seg, typeName, tTypeName, pattern Root, pattern (:/)
  , pattern (:</), Namespace(..))
import Clapi.Types.Path (Path)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Tree (TreeType(..), unbounded)
import Clapi.Types.Wire (castWireValue)
import Clapi.Protocol (Protocol, waitThen, sendFwd, sendRev)
import Clapi.TH (pathq, segq)
import Clapi.TimeDelta (tdZero, getDelta, TimeDelta(..))
import Clapi.Valuespace (apiNs, dnSeg)

class PathSegable a where
    pathNameFor :: a -> Seg

relayApiProto ::
    forall i. (Ord i, PathSegable i) =>
    i ->
    Protocol
        (ClientEvent i (TimeStamped TrDigest))
        (ClientEvent i TrDigest)
        (ServerEvent i FrDigest)
        (Either (Map Namespace i) (ServerEvent i FrDigest))
        IO ()
relayApiProto selfAddr =
    publishRelayApi >> steadyState mempty mempty
  where
    publishRelayApi = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
      rns
      mempty
      (Map.fromList $ bimap Tagged OpDefine <$>
        [ ([segq|build|], tupleDef "builddoc"
             (alSingleton [segq|commit_hash|] $ TtString "banana")
             ILUninterpolated)
        , (clock_diff, tupleDef
             "The difference between two clocks, in seconds"
             (alSingleton [segq|seconds|] $ TtFloat unbounded)
             ILUninterpolated)
        , ([segq|client_info|], structDef
             "Info about a single connected client" $ staticAl
             [ (dnSeg, (tTypeName apiNs dnSeg, Editable))
             , (clock_diff, (tTypeName rns clock_diff, ReadOnly))
             ])
        , ([segq|clients|], arrayDef "Info about the connected clients"
             Nothing
             (tTypeName rns [segq|client_info|]) ReadOnly)
        , ([segq|owner_info|], tupleDef "owner info"
             (alSingleton [segq|owner|]
               -- FIXME: want to make Ref's TypeName tagged...
               $ TtRef $ typeName rns [segq|client_info|])
             ILUninterpolated)
        , ([segq|owners|], arrayDef "ownersdoc"
             Nothing
             (tTypeName rns [segq|owner_info|]) ReadOnly)
        , ([segq|self|], tupleDef "Which client you are"
             (alSingleton [segq|info|]
               $ TtRef $ typeName rns [segq|client_info|])
             ILUninterpolated)
        , ([segq|relay|], structDef "topdoc" $ staticAl
          [ ([segq|build|], (tTypeName rns [segq|build|], ReadOnly))
          , ([segq|clients|], (tTypeName rns [segq|clients|], ReadOnly))
          , ([segq|owners|], (tTypeName rns [segq|owners|], ReadOnly))
          , ([segq|self|], (tTypeName rns [segq|self|], ReadOnly))])
        ])
      mempty
      (alFromList
        [ ([pathq|/build|], ConstChange Nothing [WireValue @Text "banana"])
        , ([pathq|/self|], ConstChange Nothing [
             WireValue $ Path.toText $ selfSeg :</ selfClientPath])
        , ( selfClientPath :/ clock_diff
          , ConstChange Nothing [WireValue @Float 0.0])
        , ( selfClientPath :/ dnSeg
          , ConstChange Nothing [WireValue @Text "Relay"])
        ])
      mempty
      mempty
    rns = Namespace [segq|relay|]
    clock_diff = [segq|clock_diff|]
    selfSeg = pathNameFor selfAddr
    selfClientPath = Root :/ [segq|clients|] :/ selfSeg
    staticAl = alFromMap . Map.fromList
    steadyState
      :: Map Seg TimeDelta -> Map Namespace Seg -> Protocol
            (ClientEvent i (TimeStamped TrDigest))
            (ClientEvent i TrDigest)
            (ServerEvent i FrDigest)
            (Either (Map Namespace i) (ServerEvent i FrDigest))
            IO ()
    steadyState timingMap ownerMap = waitThen fwd rev
      where
        fwd ce = case ce of
          ClientConnect displayName cAddr ->
            let
              cSeg = pathNameFor cAddr
              timingMap' = Map.insert cSeg tdZero timingMap
            in do
              sendFwd (ClientConnect displayName cAddr)
              pubUpdate (alFromList
                [ ( [pathq|/clients|] :/ cSeg :/ clock_diff
                  , ConstChange Nothing [WireValue $ unTimeDelta tdZero])
                , ( [pathq|/clients|] :/ cSeg :/ dnSeg
                  , ConstChange Nothing [WireValue $ Text.pack displayName])
                ])
                mempty
              steadyState timingMap' ownerMap
          ClientData cAddr (TimeStamped (theirTime, d)) -> do
            let cSeg = pathNameFor cAddr
            -- FIXME: this delta thing should probably be in the per client
            -- pipeline, it'd be less jittery and tidy this up
            delta <- lift $ getDelta theirTime
            let timingMap' = Map.insert cSeg delta timingMap
            pubUpdate (alSingleton ([pathq|/clients|] :/ cSeg :/ clock_diff)
              $ ConstChange Nothing [WireValue $ unTimeDelta delta])
              mempty
            sendFwd $ ClientData cAddr d
            steadyState timingMap' ownerMap
          ClientDisconnect cAddr ->
            sendFwd (ClientDisconnect cAddr) >> removeClient cAddr
        removeClient cAddr =
          let
            cSeg = pathNameFor cAddr
            timingMap' = Map.delete cSeg timingMap
            -- FIXME: This feels a bit like reimplementing some of the NST
            ownerMap' = Map.filter (/= cSeg) ownerMap
            (dd, dels) = ownerChangeInfo ownerMap'
          in do
            pubUpdate dd $ Map.insert ([pathq|/clients|] :/ cSeg) Nothing dels
            steadyState timingMap' ownerMap'
        pubUpdate dd dels = sendFwd $ ClientData selfAddr $ Trpd $ TrpDigest
          rns mempty mempty dels dd mempty mempty
        rev (Left ownerAddrs) = do
          let ownerMap' = pathNameFor <$> ownerAddrs
          if elem selfAddr $ Map.elems ownerAddrs
            then do
              uncurry pubUpdate $ ownerChangeInfo ownerMap'
              steadyState timingMap ownerMap'
            else
              -- The relay API did something invalid and got kicked out
              return ()
        rev (Right se) = do
          case se of
            ServerData cAddr d ->
              case d of
                Frcd frcd ->
                  sendRev $ ServerData cAddr $ Frcd
                  $ frcd {frcdData = viewAs cAddr $ frcdData frcd}
                Frpd frpd -> if frpdNamespace frpd == rns
                  then handleApiRequest frpd
                  else sendRev se
                _ -> sendRev se
            _ -> sendRev se
          steadyState timingMap ownerMap
        ownerChangeInfo
          :: Map Namespace Seg -> (DataDigest, Map Path (Maybe Attributee))
        ownerChangeInfo ownerMap' =
            ( alFromMap $ Map.mapKeys toOwnerPath $ toSetRefOp <$> ownerMap'
            , Map.mapKeys (([pathq|/owners|] :/) . unNamespace) $
                const Nothing <$>
                ownerMap `Map.difference` ownerMap')
        toOwnerPath :: Namespace -> Path
        toOwnerPath s = [pathq|/owners|] :/ unNamespace s
        toSetRefOp ns = ConstChange Nothing [
          WireValue $ Path.toText $ Root :/ selfSeg :/ [segq|clients|] :/ ns]
        viewAs i dd =
          let
            theirSeg = pathNameFor i
            theirTime = unTimeDelta $ Map.findWithDefault
              (error "Can't rewrite message for unconnected client") theirSeg
              timingMap
            alterTime (ConstChange att [wv]) = ConstChange att $ pure
              $ WireValue $ subtract theirTime $ either error id
              $ castWireValue wv
            alterTime _ = error "Weird data back out of VS"
            fiddleDataChanges p dc
              | p `Path.isChildOf` [pathq|/relay/clients|] = alterTime dc
              | p == [pathq|/relay/self|] = toSetRefOp theirSeg
              | otherwise = dc
          in
            alFmapWithKey fiddleDataChanges dd
        -- This function trusts that the valuespace has completely validated the
        -- actions the client can perform (i.e. can only change the name of a
        -- client)
        handleApiRequest (FrpDigest ns dels posts dd cops) =
          sendFwd $ ClientData selfAddr $ Trpd $
          TrpDigest ns mempty mempty dels dd cops mempty
