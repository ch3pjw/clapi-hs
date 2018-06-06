{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    DataKinds
  , FlexibleInstances
  , GADTs
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , StandaloneDeriving
#-}
module PathSpec where

import Test.Hspec
import Test.QuickCheck

import Data.List (inits)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Either (isLeft)

import Clapi.TH (ap, rp, segq)
import Clapi.Types ()
import Clapi.Types.Path
  ( Seg, mkSeg, Path(..), pattern Root, pattern (:/)
  , absPathFromText, relPathFromText
  , toText, isChildOf, joinSegs, AbsRel(..), AbsRelPath(..), Namespace(..))

import Arbitrary (smallListOf)

type CanFail = Either String

name :: Gen Seg
name = maybe [segq|__bad__|] id . mkSeg . Text.pack
  <$> smallListOf (elements ['a'..'z'])

instance Arbitrary Seg where
  arbitrary = name

deriving instance Arbitrary Namespace

instance Arbitrary (Path 'Abs) where
  arbitrary = oneof
    [ return Root
    , AbsPath <$> arbitrary <*> smallListOf name
    ]
  shrink p = case p of
    Root -> []
    AbsPath ns segs -> Root : (AbsPath ns <$> drop 1 (reverse $ inits segs))

instance Arbitrary (Path 'Rel) where
  arbitrary = RelPath <$> smallListOf name
  shrink (RelPath segs) = RelPath <$> drop 1 (reverse $ inits segs)

spec :: Spec
spec = do
    describe "Join segs" $ it "Joins segs as expected" $
          joinSegs [[segq|yo|], [segq|ho|], [segq|ahoy|]]
          `shouldBe`
          [segq|yo_ho_ahoy|]
    describe "AbsPath" $ do
        describe "From text" $ do
          it "Root" $
            "/" `shouldBeGoodPath` Root
          it "Single segment" $
            "/foo" `shouldBeGoodPath` (Root :/ [segq|foo|])
          it "Multi segment" $
            "/foo/bar" `shouldBeGoodPath` (Root :/ [segq|foo|] :/ [segq|bar|])
          -- Consider, this should be legal?
          -- it "Trailing /" $ "/foo/bar/" `shouldBeGoodPath` (Root :/ [segq|foo|] :/ [segq|bar|])
          it "Fails with trailing /" $ shouldBeBadPath (Proxy @'Abs) "/foo/bar/"
          it "Fails with no leading /" $ shouldBeBadPath (Proxy @'Abs) "foo/bar"
        describe "Quasiquoter" $
          it "Produces expected path" $
            [ap|/oi/mate|] `shouldBe` Root :/ [segq|oi|] :/ [segq|mate|]
        describe ":/" $ do
          it "Splits the end off" $ let (p :/ s) = [ap|/a/b/c|] in do
            p `shouldBe` [ap|/a/b|]
            s `shouldBe` [segq|c|]
          it "Appends a seg" $
            [ap|/a/b|] :/ [segq|c|] `shouldBe` [ap|/a/b/c|]
        describe "isChildOf" $ do
          it "Child -> True" $
            [ap|/a|] `shouldSatisfy` isChildOf [ap|/a/b/c/d|]
          it "Non-child -> False" $
            [ap|/a|] `shouldNotSatisfy` isChildOf [ap|/b/a/g|]
          it "Parent -> False" $
            [ap|/a/b|] `shouldNotSatisfy` isChildOf [ap|/a|]
          it "Same path -> True" $
            [ap|/a/b|] `shouldSatisfy` isChildOf [ap|/a/b|]
  where
    shouldBeGoodPath
      :: forall ar. AbsRelPath ar => Text -> Path ar -> Expectation
    shouldBeGoodPath t p = fromText @ar @CanFail t `shouldBe` Right p

    shouldBeBadPath
      :: forall ar. AbsRelPath ar => Proxy ar -> Text -> Expectation
    shouldBeBadPath _ t = fromText @ar @CanFail t `shouldSatisfy` isLeft
    -- rt p = (toText <$> fromText p :: CanFail Text) `shouldBe` Right p


-- pathSpec :: forall (ar :: AbsRel). Proxy ar -> Spec
-- pathSpec _ = do
--     describe "Round trip" $ do
--         it "Empty path" $ rt "/"
--         it "Top level path" $ rt "/fdoo"
--         it "Nested path" $ rt "/boo_1/chew_2"
