{-# LANGUAGE
    DataKinds
  , DeriveLift
  , GADTs
  , LambdaCase
  , KindSignatures
  , PolyKinds
  , StandaloneDeriving
#-}

module Clapi.Types.Path (
    Path(..), AbsRel(..), Seg, mkSeg, unSeg, joinSegs,
    mkAbsPath, AbsRelPath(..),
    absPathP, relPathP, segP, toText, absPathFromText, relPathFromText,
    splitHead, splitTail, parentPath,
    pattern (:/),
    isParentOf, isChildOf, isParentOfAny, isChildOfAny,
    Namespace(..), Qualified(..),
    TypeName, typeName, tTypeName, tTnNamespace, tTnName, qualify, unqualify,
    ) where

import Prelude hiding (fail)

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Fail (MonadFail, fail)
import qualified Data.Attoparsec.Text as DAT
import Data.Attoparsec.Text (Parser)
import Data.Char (isLetter, isDigit)
import Data.List (isPrefixOf)
import Data.Monoid
import Data.Tagged (Tagged(..))
import Data.Text (Text)
import qualified Data.Text as Text
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

newtype Namespace
  = Namespace {unNamespace :: Seg} deriving (Show, Eq, Ord, Lift)

data AbsRel = Abs | Rel

data Path (a :: AbsRel) where
  Root :: Path 'Abs
  AbsPath :: Namespace -> [Seg] -> Path 'Abs
  RelPath :: [Seg] -> Path 'Rel

deriving instance Eq (Path a)
deriving instance Ord (Path a)
deriving instance Lift (Path a)

mkAbsPath :: Namespace -> Path 'Rel -> Path 'Abs
mkAbsPath ns (RelPath segs) = AbsPath ns segs

-- -- You can't really write this, which is hmm...
-- pathNs :: Path 'Abs -> Maybe (Namespace, Path 'Rel)

sepChar :: Char
sepChar = '/'

sepText :: Text
sepText = Text.singleton sepChar

instance Show (Path a) where
    show = Text.unpack . toText

toText :: Path a -> Text
toText Root = sepText
toText (AbsPath ns segs) = sepText <>
  Text.intercalate sepText (unSeg (unNamespace ns) : fmap unSeg segs)
toText (RelPath segs) = Text.intercalate sepText ("." : fmap unSeg segs)

splitHead :: Path a -> Maybe (Seg, Path 'Rel)
splitHead Root = Nothing
splitHead (AbsPath ns segs) = Just (unNamespace ns, RelPath segs)
splitHead (RelPath []) = Nothing
splitHead (RelPath (s:segs)) = Just (s, RelPath segs)

splitTail :: Path a -> Maybe (Path a, Seg)
splitTail Root = Nothing
splitTail (AbsPath ns []) = Just (Root, unNamespace ns)
splitTail (AbsPath ns segs) = (\(s, ss) -> (AbsPath ns ss, s)) <$> popLast segs
splitTail (RelPath []) = Nothing
splitTail (RelPath segs) = (\(s, ss) -> (RelPath ss, s)) <$> popLast segs

popLast :: [a] -> Maybe (a, [a])
popLast [] = Nothing
popLast (x:xs) = Just $ go x xs
  where
    go y [] = (y, [])
    go y (z:zs) = (y:) <$> go z zs

pattern (:/) :: Path a -> Seg -> Path a
pattern path :/ seg <- (splitTail -> Just (path, seg)) where
  Root :/ seg = AbsPath (Namespace seg) []
  (AbsPath ns segs) :/ seg = AbsPath ns (segs ++ [seg])
  (RelPath segs) :/ seg = RelPath (segs ++ [seg])


sepP :: Parser ()
sepP = void $ DAT.char sepChar

absPathP :: Parser (Path 'Abs)
absPathP = do
  sepP
  (AbsPath
     <$> (Namespace <$> segP)
     <*> ((sepP >> segP `DAT.sepBy` sepP) <|> return [])
    ) <|> return Root

relPathP :: Parser (Path 'Rel)
relPathP = do
  _ <- DAT.char '.'
  RelPath <$> ((sepP >> segP `DAT.sepBy` sepP) <|> return [])

doParse :: MonadFail m => Parser a -> Text -> m a
doParse p = either fail return . DAT.parseOnly (p <* DAT.endOfInput)

absPathFromText :: MonadFail m => Text -> m (Path 'Abs)
absPathFromText = doParse absPathP

relPathFromText :: MonadFail m => Text -> m (Path 'Rel)
relPathFromText = doParse relPathP

class AbsRelPath ar where
  emptyPath :: Path ar
  fromText :: MonadFail m => Text -> m (Path ar)

instance AbsRelPath 'Abs where
  emptyPath = Root
  fromText = absPathFromText

instance AbsRelPath 'Rel where
  emptyPath = RelPath []
  fromText = relPathFromText

isParentOf :: Path a -> Path a -> Bool
isParentOf Root _ = True
isParentOf (AbsPath _ _) Root = False
isParentOf (AbsPath ns1 segs1) (AbsPath ns2 segs2) = ns1 == ns2 &&
  segs1 `isPrefixOf` segs2
isParentOf (RelPath segs1) (RelPath segs2) = segs1 `isPrefixOf` segs2

isChildOf :: Path a -> Path a -> Bool
isChildOf = flip isParentOf

isParentOfAny :: (Functor f, Foldable f) => Path a -> f (Path a) -> Bool
isParentOfAny parent candidates = or $ isParentOf parent <$> candidates

isChildOfAny :: (Functor f, Foldable f) => Path a -> f (Path a) -> Bool
isChildOfAny candidateChild parents = or $ isChildOf candidateChild <$> parents

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
