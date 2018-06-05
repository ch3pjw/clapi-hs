{-# LANGUAGE
    DataKinds
  , DeriveLift
  , KindSignatures
#-}

module Clapi.Types.Path (
    Path(..), AbsRel(..), Seg, mkSeg, unSeg, joinSegs,
    mkAbsPath,
    pathP, segP, toText, fromText,
    splitHead, splitTail, parentPath,
    pattern Root, pattern (:/), -- pattern (:</),
    isParentOf, isChildOf, isParentOfAny, isChildOfAny, childPaths,
    Namespace(..), Qualified(..),
    TypeName, typeName, tTypeName, tTnNamespace, tTnName, qualify, unqualify,
    ) where

import Prelude hiding (fail)
import qualified Data.Attoparsec.Text as DAT
import Data.Attoparsec.Text (Parser)
import Data.Char (isLetter, isDigit)
import Data.List (isPrefixOf)
import Data.Monoid
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.Fail (MonadFail, fail)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (Lift)

newtype Seg = Seg {unSeg :: Text} deriving (Eq, Ord, Lift)

instance Show Seg where
    show = show . unSeg

isValidSegChar :: Char -> Bool
isValidSegChar c = isLetter c || isDigit c || c == '_'

segP :: Parser Seg
segP = fmap (Seg . Text.pack) $ DAT.many1 $ DAT.satisfy isValidSegChar

mkSeg :: MonadFail m => Text -> m Seg
mkSeg = either fail return . DAT.parseOnly (segP <* DAT.endOfInput)

-- FIXME: We reckon this is a semigroup <>
joinSegs :: [Seg] -> Seg
joinSegs = Seg . Text.intercalate (Text.singleton '_') . fmap unSeg

data AbsRel = Abs | Rel

newtype Path (a :: AbsRel) = Path {unPath :: [Seg]} deriving (Eq, Ord, Lift)

mkAbsPath :: Namespace -> Path 'Rel -> Path 'Abs
mkAbsPath ns (Path segs) = Path $ unNamespace ns : segs

-- -- You can't really write this, which is hmm...
-- pathNs :: Path 'Abs -> Maybe (Namespace, Path 'Rel)

sepChar :: Char
sepChar = '/'

sepText :: Text
sepText = Text.singleton sepChar

instance Show (Path a) where
    show = Text.unpack . toText

toText :: Path a -> Text
toText (Path segs) = sepText <> Text.intercalate sepText (fmap unSeg segs)

-- FIXME: not ROot because Rel is a thing...
pattern Root :: Path a
pattern Root = Path []

splitHead :: Path a -> Maybe (Seg, Path 'Rel)
splitHead (Path []) = Nothing
splitHead (Path (seg:segs)) = Just (seg, Path segs)

-- pattern (:</) :: Seg -> Path -> Path
-- pattern seg :</ path <- (splitHead -> Just (seg, path)) where
--     seg :</ path = Path $ seg : unPath path

splitTail :: Path a -> Maybe (Path a, Seg)
splitTail (Path path) = case path of
    (y : xs) -> (\(s, ps) -> Just (Path ps, s)) $ go y xs
    [] -> Nothing
  where
    go :: Seg -> [Seg] -> (Seg, [Seg])
    go y xs = case xs of
        [] -> (y, [])
        (z : zs) -> (y :) <$> go z zs

pattern (:/) :: Path a -> Seg -> Path a
pattern path :/ seg <- (splitTail -> Just (path, seg)) where
    path :/ seg = Path $ unPath path ++ [seg]

pathP :: Parser (Path a)
pathP = let sepP = DAT.char sepChar in
    fmap Path $ sepP >> segP `DAT.sepBy` sepP

-- FIXME: This should potentially be two different parsers one for Path Abs that
-- includes the leading slash and one for Path Rel that forbids it.
fromText :: MonadFail m => Text -> m (Path a)
fromText = either fail return . DAT.parseOnly (pathP <* DAT.endOfInput)

isParentOf :: Path a -> Path a -> Bool
isParentOf (Path a) (Path b) = isPrefixOf a b

isChildOf :: Path a -> Path a -> Bool
isChildOf = flip isParentOf

isParentOfAny :: (Functor f, Foldable f) => Path a -> f (Path a) -> Bool
isParentOfAny parent candidates = or $ isParentOf parent <$> candidates

isChildOfAny :: (Functor f, Foldable f) => Path a -> f (Path a) -> Bool
isChildOfAny candidateChild parents = or $ isChildOf candidateChild <$> parents

childPaths :: Functor f => Path a -> f Seg -> f (Path a)
childPaths (Path segs) ss = Path . (segs ++) . pure <$> ss

newtype Namespace = Namespace {unNamespace :: Seg} deriving (Show, Eq, Ord)

data Qualified a
  = Qualified
  { qualifiedNs :: Namespace
  , unQualified :: a}
  deriving (Show, Eq, Ord)
type TypeName = Qualified Seg

typeName :: Namespace -> Seg -> TypeName
typeName = Qualified

tnNamespace :: TypeName -> Namespace
tnNamespace = qualifiedNs

tnName :: TypeName -> Seg
tnName = unQualified

qualify :: Namespace -> Tagged a Seg -> Tagged a TypeName
qualify ns (Tagged s) = Tagged $ typeName ns s

unqualify :: Tagged a TypeName -> (Namespace, Tagged a Seg)
unqualify (Tagged (Qualified ns s)) = (ns, Tagged s)

tTypeName :: Namespace -> Seg -> Tagged a TypeName
tTypeName ns s = Tagged $ typeName ns s

tTnNamespace :: Tagged a TypeName -> Namespace
tTnNamespace = tnNamespace . unTagged

tTnName :: Tagged a TypeName -> Tagged a Seg
tTnName = Tagged . tnName . unTagged

parentPath :: Path a -> Maybe (Path a)
parentPath p = case p of
  (pp :/ _) -> Just pp
  _ -> Nothing
