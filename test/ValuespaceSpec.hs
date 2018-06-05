{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , StandaloneDeriving
#-}
module ValuespaceSpec where

import Test.Hspec

import Data.Maybe (fromJust)
import Data.Either (either, isRight)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Fail (MonadFail)

import Clapi.TH
import Clapi.Types.AssocList (AssocList, alSingleton, alInsert)
import Clapi.Types
  ( InterpolationLimit(ILUninterpolated), WireValue(..)
  , TreeType(..), Editable(..)
  , tupleDef, structDef, arrayDef, ErrorIndex(..)
  , Definition(..)
  , StructDefinition(strDefTypes)
  , TrpDigest(..), trpDigest, DefOp(..), DataChange(..))
import qualified Clapi.Types.Path as Path
import Clapi.Types.Path
  ( Path(..), pattern (:/), pattern Root, Seg, TypeName, typeName, tTypeName
  , Namespace(..), AbsRel(..))
import Clapi.Valuespace
  ( Valuespace(..), validateVs, baseValuespace, processToRelayProviderDigest
  , processToRelayClientDigest, apiNs, vsRelinquish, ValidationErr(..))
import Clapi.Tree (treePaths, updateTreeWithDigest)
import Clapi.Tree (RoseTreeNodeType(..))

deriving instance Eq RoseTreeNodeType
deriving instance Eq ValidationErr

vsProviderErrorsOn :: Valuespace -> TrpDigest -> [Path 'Abs] -> Expectation
vsProviderErrorsOn vs d ps = case (processToRelayProviderDigest d vs) of
    Left errMap -> errMap `shouldSatisfy`
      (\em -> Set.fromList (PathError <$> ps) == Map.keysSet em)
    Right _ -> fail "Did not get expected errors"

validVersionTypeChange :: Valuespace -> TrpDigest
validVersionTypeChange vs =
  let
    svd = tupleDef
      "Stringy" (alSingleton [segq|vstr|] $ TtString "pear")
      ILUninterpolated
    rootDef = redefApiRoot
      (alInsert [segq|version|] $ tTypeName apiNs [segq|stringVersion|]) vs
  in (trpDigest apiNs)
    { trpdDefinitions = Map.fromList
      [ (Tagged [segq|stringVersion|], OpDefine svd)
      , (Tagged $ unNamespace apiNs, OpDefine rootDef)
      ]
    , trpdData = alSingleton [pathq|/version|] $
      ConstChange Nothing [WireValue ("pear" :: Text)]
    }

vsAppliesCleanly :: MonadFail m => TrpDigest -> Valuespace -> m Valuespace
vsAppliesCleanly d vs = either (fail . show) (return . snd) $
  processToRelayProviderDigest d vs

redefApiRoot
  :: (AssocList Seg (Tagged Definition TypeName)
      -> AssocList Seg (Tagged Definition TypeName))
  -> Valuespace -> Definition
redefApiRoot f vs = structDef "Frigged by test" $ (, ReadOnly) <$> f currentKids
  where
    currentKids = fmap fst $ grabDefTypes $ fromJust $
      Map.lookup apiNs (vsTyDefs vs) >>= Map.lookup (Tagged $ unNamespace apiNs)
    grabDefTypes (StructDef sd) = strDefTypes sd
    grabDefTypes _ = error "API ns root type not a struct!"

extendedVs :: MonadFail m => Definition -> Seg -> DataChange -> m Valuespace
extendedVs def s dc =
  let
    rootDef = redefApiRoot (alInsert s $ tTypeName apiNs s) baseValuespace
    d = (trpDigest apiNs)
      { trpdDefinitions = Map.fromList
        [ (Tagged s, OpDefine def)
        , (Tagged $ unNamespace apiNs, OpDefine rootDef)]
      , trpdData = alSingleton (Root :/ s) dc
      }
  in vsAppliesCleanly d baseValuespace

vsWithXRef :: MonadFail m => m Valuespace
vsWithXRef =
  let
    newNodeDef = tupleDef
      "for test"
      (alSingleton [segq|daRef|] $ TtRef $
        typeName (Namespace [segq|api|]) [segq|version|])
      ILUninterpolated
    newVal = ConstChange Nothing [WireValue $ Path.toText [pathq|/api/version|]]
  in extendedVs newNodeDef refSeg newVal

refSeg :: Seg
refSeg = [segq|ref|]

emptyArrayD :: Seg -> Valuespace -> TrpDigest
emptyArrayD s vs = (trpDigest apiNs)
    { trpdDefinitions = Map.fromList
      [ (Tagged s, OpDefine vaDef)
      , (Tagged $ unNamespace apiNs, OpDefine rootDef)]
    }
  where
    vaDef = arrayDef
      "for test" Nothing (tTypeName apiNs [segq|version|]) Editable
    -- FIXME: is vs always baseValuespace?
    rootDef = redefApiRoot (alInsert s $ tTypeName apiNs s) vs

spec :: Spec
spec = do
  describe "Validation" $ do
    it "baseValuespace valid" $
      let
        allTainted = Map.fromList $ fmap (,Nothing) $ treePaths Root $
          vsTree baseValuespace
        validated = either (error . show) snd $
          validateVs allTainted baseValuespace
      in do
        validated `shouldBe` baseValuespace
    it "rechecks on data changes" $
      let
        d = (trpDigest apiNs)
          { trpdData = alSingleton [pathq|/version|] $
            ConstChange Nothing [WireValue @Text "wrong"]
          }
      in vsProviderErrorsOn baseValuespace d [[pathq|/api/version|]]
    it "rechecks on type def changes" $
      -- Make sure changing (api, version) goes and checks things defined
      -- to have that type:
      let
          newDef = tupleDef
            "for test"
            (alSingleton [segq|versionString|] $ TtString "apple")
            ILUninterpolated
          d = (trpDigest apiNs)
            { trpdDefinitions = Map.singleton (Tagged [segq|version|]) $
              OpDefine newDef
            }
      in vsProviderErrorsOn baseValuespace d [[pathq|/api/version|]]
    it "rechecks on container ops" $
      let
        d = (trpDigest apiNs)
          { trpdDeletes = Map.singleton (Root :/ [segq|version|]) Nothing }
      in vsProviderErrorsOn baseValuespace d [[pathq|/api|]]
    it "should only re-validate data that has been marked as invalid" $
      let
        p = [pathq|/api/version|]
        badVs = baseValuespace {
          vsTree = snd $ updateTreeWithDigest mempty
            (alSingleton p $ ConstChange Nothing []) $
            vsTree baseValuespace}
        invalidatedPaths = Map.singleton p Nothing
      in do
        -- Validation without specifying the change should miss the bad data:
        either (error . show) snd (validateVs mempty badVs) `shouldBe` badVs
        -- Validation explicitly asking to revalidate the change should fail:
        either id (error . show) (validateVs invalidatedPaths badVs)
          `shouldSatisfy` (not . null)
    it "can change the version type" $
      (
        vsAppliesCleanly (validVersionTypeChange baseValuespace) baseValuespace
        :: Either String Valuespace)
      `shouldSatisfy` isRight
    it "xref referee type change errors" $ do
      -- Change the type of the instance referenced in a cross reference
      vs <- vsWithXRef
      vsProviderErrorsOn vs (validVersionTypeChange vs)
        [Root :/ unNamespace apiNs :/ refSeg]
    it "xref old references do not error" $
      let
        v2s = [segq|v2|]
        v2Val = alSingleton (Root :/ v2s) $ ConstChange Nothing
          [WireValue @Word32 1, WireValue @Word32 2, WireValue @Int32 3]
      in do
        vs <- vsWithXRef
        -- Add another version node:
        let v2ApiDef = redefApiRoot
              (alInsert v2s $ tTypeName apiNs [segq|version|]) vs
        vs' <- vsAppliesCleanly (trpDigest apiNs)
          { trpdDefinitions = Map.singleton (Tagged $ unNamespace apiNs) $
            OpDefine v2ApiDef
          , trpdData = v2Val
          }
          vs
        -- Update the ref to point at new version:
        vs'' <- vsAppliesCleanly (trpDigest apiNs)
          { trpdData = alSingleton (Root :/ refSeg) $
            ConstChange Nothing [WireValue $ Path.toText [pathq|/api/v2|]]
          }
          vs'
        (vsAppliesCleanly (validVersionTypeChange vs'') vs''
          :: Either String Valuespace) `shouldSatisfy` isRight
    it "Array" $
      let
        ars = [segq|arr|]
        badChild = (trpDigest apiNs)
          { trpdData = alSingleton [pathq|/arr/bad|] $
            ConstChange Nothing [WireValue @Text "boo"]
          }
        goodChild = (trpDigest apiNs)
          { trpdData = alSingleton [pathq|/arr/mehearties|] $
            ConstChange Nothing
            [WireValue @Word32 3, WireValue @Word32 4, WireValue @Int32 3]
          }
        removeGoodChild = (trpDigest apiNs)
          {trpdDeletes = Map.singleton [pathq|/arr/mehearties|] Nothing}
      in do
        vs <- vsAppliesCleanly (emptyArrayD ars baseValuespace) baseValuespace
        vsProviderErrorsOn vs badChild [[pathq|/api/arr/bad|]]
        vs' <- vsAppliesCleanly goodChild vs
        vs'' <- vsAppliesCleanly removeGoodChild vs'
        vs'' `shouldBe` vs
    it "Errors on struct with missing child" $
      let
        rootDef = redefApiRoot
          (alInsert [segq|unfilled|] $ tTypeName apiNs [segq|version|])
          baseValuespace
        missingChild = (trpDigest apiNs)
          { trpdDefinitions = Map.singleton (Tagged $ unNamespace apiNs) $
                OpDefine rootDef}
      in vsProviderErrorsOn baseValuespace missingChild [[pathq|/api|]]
    it "Relinquish" $
      let
        fs = [segq|foo|]
        fooRootDef = arrayDef "frd" Nothing (tTypeName apiNs [segq|version|])
          ReadOnly
        claimFoo = (trpDigest $ Namespace fs)
          {trpdDefinitions = Map.singleton (Tagged fs) $ OpDefine fooRootDef}
      in do
        vs <- vsAppliesCleanly claimFoo baseValuespace
        vsRelinquish (Namespace fs) vs `shouldBe` baseValuespace
    describe "Client" $
        it "Can create new array entries" $
          let
            dd = alSingleton [pathq|/api/arr/a|] $ ConstChange Nothing
                [WireValue @Word32 1, WireValue @Word32 2, WireValue @Int32 3]
          in do
            vs <- vsAppliesCleanly (emptyArrayD [segq|arr|] baseValuespace) baseValuespace
            processToRelayClientDigest mempty dd vs `shouldBe` mempty
