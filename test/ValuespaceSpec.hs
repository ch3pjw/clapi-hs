{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , PatternSynonyms
  , QuasiQuotes
  , StandaloneDeriving
#-}
module ValuespaceSpec where

import Test.Hspec

import Control.Lens (over, set, view)
import Data.Maybe (fromJust)
import Data.Either (either, isRight, isLeft)
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import Data.Word
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Fail (MonadFail)

import qualified Data.Map.Mol as Mol

import Clapi.TH
import Clapi.Types.AssocList
  ( AssocList, alSingleton, alEmpty, alInsert, alFromList)
import Clapi.Types
  ( InterpolationLimit(ILUninterpolated)
  , WireType(..), WireValue(..), someWv, someWireable
  , TreeType(..), SomeTreeType(..), someTv, ttString, ttRef, unbounded
  , Editable(..), tupleDef, structDef, arrayDef, DataErrorIndex(..)
  , DefName, Definition(..), SomeDefinition(..), withDefinition, MetaType(..)
  , TrpDigest(..), DefOp(..), DataChange(..)
  , TrcUpdateDigest(..), trcudEmpty)
import qualified Clapi.Types.Path as Path
import Clapi.Types.Path
  ( Path, pattern (:/), pattern Root, Seg, Namespace(..))
import Clapi.Valuespace
  ( Valuespace(..), vsTyDefs, vsTree, baseValuespace
  , processTrpd, processTrcud)
import Clapi.Tree
  ( RoseTree(..), RoseTreeNodeType(..), treePaths)
import Clapi.Types.SequenceOps (SequenceOp(..))

-- deriving instance Eq RoseTreeNodeType
-- -- deriving instance Eq ValidationErr

-- -- | Fully revalidates the given Valuespace and throws an error if there are any
-- --   validation issues.
-- unsafeValidateVs :: Valuespace -> Valuespace
-- unsafeValidateVs vs = either (error . show) snd $ validateVs allTainted vs
--   where
--     allTainted = Map.fromList $ fmap (,Nothing) $ treePaths Root $ view vsTree vs


-- testS :: Seg
-- testS = [segq|test|]
-- testNs :: Namespace
-- testNs = Namespace testS

-- versionS :: Seg
-- versionS = [segq|version|]

-- testValuespace :: Valuespace
-- testValuespace = let tt = TtInt32 unbounded in unsafeValidateVs $
--   set vsTyDefs (
--       Map.fromList
--           [ (Tagged testS, structDef "test root" $ alFromList
--               [ (versionS, (Tagged versionS, ReadOnly))
--               ])
--           , (Tagged versionS, tupleDef
--               "versioney"
--               (alSingleton versionS $ SomeTreeType tt) ILUninterpolated)
--           ]) $
--   set vsTree (
--       RtContainer $ alSingleton versionS
--           (Nothing, RtConstData Nothing [someTv tt 3])) $
--   baseValuespace (Tagged testS) Editable

-- vsProviderErrorsOn :: Valuespace -> TrpDigest -> [Path] -> Expectation
-- vsProviderErrorsOn vs d ps = case processTrpd d vs of
--     Left errMap -> Mol.keysSet errMap `shouldBe` Set.fromList (PathError <$> ps)
--     Right _ -> fail "Did not get expected errors"

-- vsClientErrorsOn :: Valuespace -> TrcUpdateDigest -> [Path] -> Expectation
-- vsClientErrorsOn vs d ps = let (errMap, _) = processTrcUpdateDigest vs d in
--   if (null errMap)
--     then fail "Did not get expected errors"
--     else Mol.keysSet errMap `shouldBe` Set.fromList (PathError <$> ps)

-- validVersionTypeChange :: Valuespace -> TrpDigest
-- validVersionTypeChange vs =
--   let
--     svd = tupleDef
--       "Stringy" (alSingleton [segq|vstr|] $ ttString "pear")
--       ILUninterpolated
--     rootDef = redefTestRoot
--       (alInsert versionS $ Tagged [segq|stringVersion|]) vs
--   in TrpDigest
--     testNs
--     mempty
--     (Map.fromList
--       [ (Tagged [segq|stringVersion|], OpDefine svd)
--       , (Tagged $ unNamespace testNs, OpDefine rootDef)
--       ])
--     (alSingleton [pathq|/version|]
--       $ ConstChange Nothing [someWv WtString "pear"])
--     mempty
--     mempty

-- vsAppliesCleanly :: MonadFail m => TrpDigest -> Valuespace -> m Valuespace
-- vsAppliesCleanly d vs = either (fail . show) (return . snd) $ processTrpd d vs

-- redefTestRoot
--   :: (AssocList Seg (Tagged SomeDefinition Seg)
--       -> AssocList Seg (Tagged SomeDefinition Seg))
--   -> Valuespace -> SomeDefinition
-- redefTestRoot f vs =
--     structDef "Frigged by test" $ (, ReadOnly) <$> f currentKids
--   where
--     currentKids = fmap fst $ withDefinition strDefChildTys' $ fromJust $
--       Map.lookup (Tagged $ unNamespace testNs) $ view vsTyDefs vs
--     strDefChildTys' :: Definition mt -> AssocList Seg (DefName, Editable)
--     strDefChildTys' sd@(StructDef {}) = strDefChildTys sd
--     strDefChildTys' _ = error "Test vs root type not a struct!"

-- extendedVs :: MonadFail m => SomeDefinition -> Seg -> DataChange -> m Valuespace
-- extendedVs def s dc =
--   let
--     rootDef = redefTestRoot (alInsert s $ Tagged s) testValuespace
--     d = TrpDigest
--       testNs
--       mempty
--       (Map.fromList
--         [ (Tagged s, OpDefine def)
--         , (Tagged $ unNamespace testNs, OpDefine rootDef)])
--       (alSingleton (Root :/ s) dc)
--       mempty
--       mempty
--   in vsAppliesCleanly d testValuespace

-- vsWithXRef :: MonadFail m => m Valuespace
-- vsWithXRef =
--   let
--     newNodeDef = tupleDef
--       "for test"
--       -- FIXME: Should the ref seg be tagged?:
--       (alSingleton [segq|daRef|] $ ttRef versionS)
--       ILUninterpolated
--     newVal = ConstChange Nothing
--       [someWireable $ Path.toText Path.unSeg [pathq|/version|]]
--   in extendedVs newNodeDef refSeg newVal

-- refSeg :: Seg
-- refSeg = [segq|ref|]

-- emptyArrayD :: Seg -> Valuespace -> TrpDigest
-- emptyArrayD s vs = TrpDigest
--     testNs
--     mempty
--     (Map.fromList
--      [ (Tagged s, OpDefine vaDef)
--      , (Tagged $ unNamespace testNs, OpDefine rootDef)])
--     alEmpty
--     mempty
--     mempty
--   where
--     vaDef = arrayDef "for test" Nothing (Tagged [segq|version|]) Editable
--     -- FIXME: is vs always testValuespace?
--     rootDef = redefTestRoot (alInsert s $ Tagged s) vs

spec :: Spec
spec = return ()
-- spec = do
--   describe "Validation" $ do
--     it "raw baseValuespace invalid" $
--       let
--         rawValuespace = baseValuespace (Tagged testS) Editable
--         allTainted = Map.fromList $ fmap (,Nothing) $ treePaths Root $
--           view vsTree rawValuespace
--       in validateVs allTainted rawValuespace `shouldSatisfy` isLeft
--     it "rechecks on data changes" $
--       let
--         d = TrpDigest testNs mempty mempty
--           (alSingleton [pathq|/version|] $
--            ConstChange Nothing [someWv WtString "wrong"])
--           mempty mempty
--       in vsProviderErrorsOn testValuespace d [[pathq|/version|]]
--     it "rechecks on type def changes" $
--       -- Make sure changing (api, version) goes and checks things defined
--       -- to have that type:
--       let
--           newDef = tupleDef
--             "for test"
--             (alSingleton [segq|versionString|] $ ttString "apple")
--             ILUninterpolated
--           d = TrpDigest
--             testNs mempty
--             (Map.singleton (Tagged versionS) $ OpDefine newDef)
--             alEmpty mempty mempty
--       in vsProviderErrorsOn testValuespace d [[pathq|/version|]]
--     it "rechecks on container ops" $
--       let
--         d = TrpDigest
--             testNs
--             mempty
--             mempty
--             alEmpty
--             (Map.singleton Root $ Map.singleton [segq|version|] (Nothing, SoAbsent))
--             mempty
--       in vsProviderErrorsOn testValuespace d [Root]
--     it "should only re-validate data that has been marked as invalid" $
--       let
--         p = [pathq|/api/version|]
--         badVs = over vsTree
--           (snd . updateTreeWithDigest mempty
--            (alSingleton p $ ConstChange Nothing []))
--           testValuespace
--         invalidatedPaths = Map.singleton p Nothing
--       in do
--         -- Validation without specifying the change should miss the bad data:
--         either (error . show) snd (validateVs mempty badVs) `shouldBe` badVs
--         -- Validation explicitly asking to revalidate the change should fail:
--         either id (error . show) (validateVs invalidatedPaths badVs)
--           `shouldSatisfy` (not . null)
--     it "can change the version type" $
--       (
--         vsAppliesCleanly (validVersionTypeChange testValuespace) testValuespace
--         :: Either String Valuespace)
--       `shouldSatisfy` isRight
--     it "xref referee type change errors" $ do
--       -- Change the type of the instance referenced in a cross reference
--       vs <- vsWithXRef
--       vsProviderErrorsOn vs (validVersionTypeChange vs)
--         [Root :/ refSeg]
--     it "xref old references do not error" $
--       let
--         v2s = [segq|v2|]
--         v2Val = alSingleton (Root :/ v2s) $ ConstChange Nothing
--           [someWv WtInt32 123]
--       in do
--         vs <- vsWithXRef
--         -- Add another version node:
--         let v2ApiDef = redefTestRoot
--               (alInsert v2s $ Tagged [segq|version|]) vs
--         vs' <- vsAppliesCleanly
--           (TrpDigest testNs mempty
--             (Map.singleton (Tagged $ unNamespace testNs) $ OpDefine v2ApiDef)
--             v2Val mempty mempty)
--           vs
--         -- Update the ref to point at new version:
--         vs'' <- vsAppliesCleanly
--           (TrpDigest testNs mempty mempty
--             (alSingleton (Root :/ refSeg)
--              $ ConstChange Nothing
--              [someWireable $ Path.toText Path.unSeg [pathq|/v2|]])
--             mempty mempty)
--           vs'
--         (vsAppliesCleanly (validVersionTypeChange vs'') vs''
--           :: Either String Valuespace) `shouldSatisfy` isRight
--     it "Array" $
--       let
--         ars = [segq|arr|]
--         badChild = TrpDigest
--           testNs
--           mempty
--           mempty
--           (alSingleton [pathq|/arr/bad|] $
--             ConstChange Nothing [someWv WtString "boo"])
--           mempty
--           mempty
--         goodChild = TrpDigest
--           testNs
--           mempty
--           mempty
--           (alSingleton [pathq|/arr/mehearties|] $
--             ConstChange Nothing [someWv WtInt32 3])
--           mempty
--           mempty
--         removeGoodChild = TrpDigest
--           testNs
--           mempty
--           mempty
--           alEmpty
--           (Map.singleton [pathq|/arr|] $ Map.singleton [segq|mehearties|] (Nothing, SoAbsent))
--           mempty
--       in do
--         vs <- vsAppliesCleanly (emptyArrayD ars testValuespace) testValuespace
--         vsProviderErrorsOn vs badChild [[pathq|/arr/bad|]]
--         vs' <- vsAppliesCleanly goodChild vs
--         vs'' <- vsAppliesCleanly removeGoodChild vs'
--         vs'' `shouldBe` vs
--     it "Errors on struct with missing child" $
--       let
--         rootDef = redefTestRoot
--           (alInsert [segq|unfilled|] $ Tagged [segq|version|])
--           testValuespace
--         missingChild = TrpDigest
--           testNs
--           mempty
--           (Map.singleton (Tagged $ unNamespace testNs) $ OpDefine rootDef)
--           alEmpty
--           mempty
--           mempty
--       in vsProviderErrorsOn testValuespace missingChild [Root]
--     describe "Client" $
--         it "Cannot itself create new array entries" $
--           let
--             dd = alSingleton [pathq|/arr/a|] $ ConstChange Nothing
--                 [someWv WtWord32 1, someWv WtWord32 2, someWv WtInt32 3]
--             trcud = (trcudEmpty testNs) {trcudData = dd}
--           in do
--             vs <- vsAppliesCleanly (emptyArrayD [segq|arr|] testValuespace) testValuespace
--             vsClientErrorsOn vs trcud [[pathq|/arr/a|]]
