{-# LANGUAGE FlexibleInstances #-}
module Clapi.Serialisation
    (
      encode,
      decode,
      parser,
      typeTag,
      typeTags,
      typeFromTag,
      valueTag
    ) where

import Data.List (intersect)
import Data.Char (chr)
import Data.Monoid ((<>), mconcat, Sum(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM2)
import Control.Monad.Fail (MonadFail)
import qualified Data.ByteString as B
import Data.Word (Word8, Word16, Word32, Word64)
import Blaze.ByteString.Builder (
    Builder, toByteString, fromInt32be, fromInt64be, fromWord8, fromWord16be,
    fromWord32be, fromWord64be)
import Data.ByteString.Builder(floatBE, doubleBE)
import Blaze.ByteString.Builder.ByteString (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromText, fromString)
import Data.ByteString.UTF8 (toString)
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)

import Data.Attoparsec.ByteString (Parser, parseOnly, count, anyWord8, satisfy, inClass)
import Data.Attoparsec.Binary (anyWord16be, anyWord32be, anyWord64be)
import qualified Data.Attoparsec.ByteString as APBS
import qualified Data.Attoparsec.Text as APT

import Clapi.Types(
    CanFail, ClapiTypeEnum(..), ClapiValue(..), clapiValueType, Bundle(..),
    Message(..), ClapiMethod(..), Time(..), Interpolation(..),
    UMsgError(..), SubMessage(..), DataUpdateMessage(..),
    TreeUpdateMessage(..), OwnerUpdateMessage(..))
import qualified Clapi.Path as Path
import qualified Path.Parsing as Path
import Clapi.Parsing (methodToString, methodParser)
import Clapi.Util (composeParsers)

(<<>>) = liftM2 (<>)

encode :: Serialisable a => a -> CanFail B.ByteString
encode x = toByteString <$> builder x

decode :: Serialisable a => B.ByteString -> CanFail a
decode = parseOnly parser

class Serialisable a where
    builder :: a -> CanFail Builder
    parser :: Parser a

instance Serialisable Word16 where
    builder = return . fromWord16be
    parser = anyWord16be

instance Serialisable Word32 where
    builder = return . fromWord32be
    parser = anyWord32be

instance Serialisable Word64 where
    builder = return . fromWord64be
    parser = anyWord64be

instance Serialisable a => Serialisable (Sum a) where
    builder (Sum i) = builder i
    parser = Sum <$> parser

prefixLength :: Builder -> CanFail Builder
prefixLength b = (lenBuilder byteSize) <<>> (return b) where
    lenBuilder x
      | x <= fromIntegral (maxBound :: Word16) =
          builder $ (fromIntegral x :: Word16)
      | otherwise = Left "Too long"
    byteSize = B.length $ toByteString b

decodeLengthPrefixedBytes :: (B.ByteString -> b) -> Parser b
decodeLengthPrefixedBytes decoder = do
    len <- parser :: Parser Word16
    bytes <- APBS.take $ fromIntegral len
    return $ decoder bytes

instance Serialisable String where
    builder = prefixLength . fromString
    parser = decodeLengthPrefixedBytes toString

decodeUtf8With' = decodeUtf8With onError
  where
    onError :: String -> Maybe Word8 -> Maybe Char
    onError s Nothing = Nothing  -- End of input
    onError s (Just c) = Just '?'  -- Undecodable

instance Serialisable T.Text where
    builder = prefixLength . fromText
    parser = decodeLengthPrefixedBytes decodeUtf8With'

instance Serialisable Char where
    builder = return . fromChar
    parser = T.head . decodeUtf8With' <$> APBS.take 1

instance Serialisable Time where
    builder (Time x y) = return (fromWord64be x <> fromWord32be y)
    parser = foo <$> (fromIntegral <$> anyWord64be) <*> (fromIntegral <$> anyWord32be)
      where
        foo x y = Time x y

instance Serialisable Path.Path where
    builder = builder . Path.toString
    parser = composeParsers parser Path.pathP

instance Serialisable ClapiMethod where
    builder = builder . methodToString
    parser = composeParsers parser methodParser


typeTag :: ClapiTypeEnum -> Char
typeTag ClTTime = 't'
typeTag ClTEnum = 'e'
typeTag ClTWord32 = 'u'
typeTag ClTWord64 = 'U'
typeTag ClTInt32 = 'i'
typeTag ClTInt64 = 'I'
typeTag ClTFloat = 'd'
typeTag ClTDouble = 'D'
typeTag ClTString = 's'
typeTag ClTList = 'l'

typeMapping :: [(Char, ClapiTypeEnum)]
typeMapping = (\cte -> (typeTag cte, cte)) <$> [minBound ..]

typeTags :: [Char]
typeTags = fst <$> typeMapping

valueTag :: ClapiValue -> Char
valueTag = typeTag . clapiValueType

typeFromTag :: (MonadFail m) => Char -> m ClapiTypeEnum
typeFromTag t = case lookup t typeMapping of
    Just e -> return e
    Nothing -> fail $ "Unrecognised type tag: '" ++ [t] ++ "'"

cvBuilder :: ClapiValue -> CanFail Builder
cvBuilder (ClTime t) = builder t
cvBuilder (ClEnum x) = return $ fromWord8 x
cvBuilder (ClWord32 x) = return $ fromWord32be x
cvBuilder (ClWord64 x) = return $ fromWord64be x
cvBuilder (ClInt32 x) = return $ fromInt32be x
cvBuilder (ClInt64 x) = return $ fromInt64be x
cvBuilder (ClFloat x) = return $ floatBE x
cvBuilder (ClDouble x) = return $ doubleBE x
cvBuilder (ClString x) = builder x
cvBuilder (ClList vs) = builder vs


taggedEncode :: (Monoid b, Serialisable b) =>
    (a -> b) -> (a -> CanFail Builder) -> [a] -> CanFail Builder
taggedEncode derive build xs = derived <<>> built where
    built = foldl (<<>>) (return mempty) (map build xs)
    derived = builder $ foldl (<>) mempty (map derive xs)

parseTags :: APT.Parser String
parseTags = APT.many' $ APT.satisfy (APT.inClass typeTags)

instance Serialisable [ClapiValue] where
    builder = taggedEncode derive cvBuilder where
        derive cv = [valueTag cv]
    parser = do
        types <- composeParsers parser parseTags >>= mapM typeFromTag
        sequence $ map (cvParser) types
      where
        cvParser :: ClapiTypeEnum -> Parser ClapiValue
        cvParser ClTTime = ClTime <$> (parser :: Parser Time)
        cvParser ClTEnum = ClEnum <$> anyWord8
        cvParser ClTWord32 = ClWord32 <$> anyWord32be
        cvParser ClTWord64 = ClWord64 <$> anyWord64be
        cvParser ClTInt32 = ClInt32 <$> fromIntegral <$> anyWord32be
        cvParser ClTInt64 = ClInt64 <$> fromIntegral <$> anyWord64be
        cvParser ClTFloat = ClFloat <$> wordToFloat <$> anyWord32be
        cvParser ClTDouble = ClDouble <$> wordToDouble <$> anyWord64be
        cvParser ClTString = ClString <$> (parser :: Parser T.Text)
        cvParser ClTList = ClList <$> (parser :: Parser [ClapiValue])


encodeListN :: (Serialisable a) => [a] -> CanFail Builder
encodeListN = taggedEncode (const (1 :: Sum Word16)) builder

parseListN :: (Serialisable a) => Parser [a]
parseListN = do
    len <- parser :: Parser Word16
    count (fromIntegral len) parser

instance Serialisable [Message] where
    builder = encodeListN where
    parser = parseListN

data TaggedData e a = TaggedData {
    tdEnumToTag :: e -> Char,
    tdTagToEnum :: Char -> CanFail e,
    tdAllTags :: [Char],
    tdTypeToEnum :: a -> e,
    tdBuilder :: a -> CanFail Builder,
    tdParser :: e -> Parser a}

tdTotalParser :: TaggedData e a -> Parser a
tdTotalParser td = do
    t <- satisfy (inClass $ tdAllTags td)
    let te = case tdTagToEnum td $ chr $ fromIntegral t of
            (Left m) -> error m  -- Should never get here!
            (Right te) -> te
    tdParser td te

tdTotalBuilder :: TaggedData e a -> a -> CanFail Builder
tdTotalBuilder td b = builder (tdEnumToTag td $ (tdTypeToEnum td) b) <<>> (tdBuilder td) b

genTagged :: (Enum e, Bounded e) => (e -> Char) -> (a -> e) -> (a -> CanFail Builder) -> (e -> Parser a) -> TaggedData e a
genTagged toTag typeToEnum b p = TaggedData toTag fromTag allTags typeToEnum b p
  where
    tagMap = (\ei -> (toTag ei, ei)) <$> [minBound ..]
    allTags = fst <$> tagMap
    fromTag t = case lookup t tagMap of
        Just e -> return e
        Nothing -> fail $ "Unrecognised tag: '" ++ [t] ++ "' expecting one of '" ++ allTags ++ "'"

instance Serialisable UMsgError where
    builder (UMsgError p s) = builder p <<>> builder s
    parser = do
        p <- parser
        s <- parser
        return $ UMsgError p s

data SubMsgType
  = SubMsgTSub
  | SubMsgTUnsub deriving (Enum, Bounded)

subMsgTaggedData = genTagged typeToTag msgToType (builder . subMsgPath) msgParser
  where
    typeToTag (SubMsgTSub) = 'S'
    typeToTag (SubMsgTUnsub) = 'U'
    msgToType (UMsgSubscribe _) = SubMsgTSub
    msgToType (UMsgUnsubscribe _) = SubMsgTUnsub
    msgParser (SubMsgTSub) = UMsgSubscribe <$> parser
    msgParser (SubMsgTUnsub) = UMsgUnsubscribe <$> parser

instance Serialisable SubMessage where
    builder = tdTotalBuilder subMsgTaggedData
    parser = tdTotalParser subMsgTaggedData

data DataUpdateMsgType
  = DUMTAdd
  | DUMTSet
  | DUMTRemove
  | DUMTClear
  | DUMTSetChildren deriving (Enum, Bounded)

dumtTaggedData = genTagged typeToTag msgToType msgBuilder msgParser
  where
    typeToTag (DUMTAdd) = 'a'
    typeToTag (DUMTSet) = 's'
    typeToTag (DUMTRemove) = 'r'
    typeToTag (DUMTClear) = 'c'
    typeToTag (DUMTSetChildren) = 'C'
    msgToType (UMsgAdd _ _ _ _ _ _) = DUMTAdd
    msgToType (UMsgSet _ _ _ _ _ _) = DUMTSet
    msgToType (UMsgRemove _ _ _ _) = DUMTRemove
    msgToType (UMsgClear _ _ _ _) = DUMTClear
    msgToType (UMsgSetChildren _ _ _) = DUMTSetChildren
    msgBuilder (UMsgAdd p t v i a s) = builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>> builder s
    msgBuilder (UMsgSet p t v i a s) = builder p <<>> builder t <<>> builder v <<>> builder i <<>> builder a <<>> builder s
    msgBuilder (UMsgRemove p t a s) = builder p <<>> builder t <<>> builder a <<>> builder s
    msgBuilder (UMsgClear p t a s) = builder p <<>> builder t <<>> builder a <<>> builder s
    msgBuilder (UMsgSetChildren p ns a) = builder p <<>> encodeListN ns <<>> builder a
    sap mt = do
        p <- parser
        t <- parser
        v <- parser
        i <- parser
        a <- parser
        s <- parser
        return $ mt p t v i a s
    rcp mt = do
        p <- parser
        t <- parser
        a <- parser
        s <- parser
        return $ mt p t a s
    msgParser (DUMTAdd) = sap UMsgAdd
    msgParser (DUMTSet) = sap UMsgSet
    msgParser (DUMTRemove) = rcp UMsgRemove
    msgParser (DUMTClear) = rcp UMsgClear
    msgParser (DUMTSetChildren) = do
        p <- parser
        ns <- parseListN
        a <- parser
        return $ UMsgSetChildren p ns a

instance Serialisable DataUpdateMessage where
    builder = tdTotalBuilder dumtTaggedData
    parser = tdTotalParser dumtTaggedData

data TUMT
  = TUMTAssignType
  | TUMTDelete deriving (Enum, Bounded)

tumtTaggedData = genTagged typeToTag msgToType msgBuilder msgParser
  where
    typeToTag (TUMTAssignType) = 'A'
    typeToTag (TUMTDelete) = 'D'
    msgToType (UMsgAssignType _ _) = TUMTAssignType
    msgToType (UMsgDelete _) = TUMTDelete
    msgBuilder (UMsgAssignType p tp) = builder p <<>> builder tp
    msgBuilder (UMsgDelete p) = builder p
    msgParser (TUMTAssignType) = do
        p <- parser
        tp <- parser
        return $ UMsgAssignType p tp
    msgParser (TUMTDelete) = UMsgDelete <$> parser

eitherTagged :: TaggedData e a -> TaggedData f b -> TaggedData Char (Either a b)
eitherTagged a b = case intersect (tdAllTags a) (tdAllTags b) of
    [] -> TaggedData toTag fromTag allTags typeToEnum cb cp
    i -> error $ "Tags overlap: " ++ i
  where
    allTags = (tdAllTags a) ++ (tdAllTags b)
    toTag = id
    fromTag t = case t `elem` allTags of
        True -> return t
        False -> fail "Bad tag"
    typeToEnum = either (charFor a) (charFor b)
    charFor sub = (tdEnumToTag sub) . (tdTypeToEnum sub)
    cb = either (tdBuilder a) (tdBuilder b)
    cp t = if t `elem` (tdAllTags a) then Left <$> subParse a t else Right <$> subParse b t
    subParse sub t = case tdTagToEnum sub t of
        Left m -> error m
        Right e -> tdParser sub e

oumTaggedData = eitherTagged tumtTaggedData dumtTaggedData

instance Serialisable OwnerUpdateMessage where
    builder = tdTotalBuilder oumTaggedData
    parser = tdTotalParser oumTaggedData

data BundleTypeEnum
  = RequestBundleType
  | UpdateBundleType deriving (Enum, Bounded)

bundleTaggedData = genTagged typeToTag bundleType bundleBuilder bundleParser
  where
    typeToTag (RequestBundleType) = 'r'
    typeToTag (UpdateBundleType) = 'u'
    bundleType (RequestBundle _ _) = RequestBundleType
    bundleType (UpdateBundle _ _) = UpdateBundleType
    bundleBuilder (RequestBundle subs dums) = encodeListN subs <<>> encodeListN dums
    bundleBuilder (UpdateBundle errs oums) = encodeListN errs <<>> encodeListN oums
    bundleParser (RequestBundleType) = do
        s <- parseListN
        u <- parseListN
        return $ RequestBundle s u
    bundleParser (UpdateBundleType) = do
        e <- parseListN
        u <- parseListN
        return $ UpdateBundle e u

instance Serialisable Bundle where
    builder = tdTotalBuilder bundleTaggedData
    parser = tdTotalParser bundleTaggedData


badTag :: (MonadFail m) => String -> Char ->  m a
badTag n c = fail $ "Bad " ++ n ++ " type tag '" ++ (show c) ++ "'"


instance Serialisable Message where
    builder (MsgError p m) = builder 'E' <<>> builder p <<>> builder m
    builder (MsgSet p time cvs i ma ms) =
        builder 's' <<>> builder p <<>> builder time <<>> builder cvs <<>>
        builder i <<>> builder ma <<>> builder ms
    builder (MsgAdd p time cvs i ma ms) =
        builder 'a' <<>> builder p <<>> builder time <<>> builder cvs <<>>
        builder i <<>> builder ma <<>> builder ms
    builder (MsgRemove p time ma ms) =
        builder 'r' <<>> builder p <<>> builder time
    builder (MsgClear p time ma ms) =
        builder 'c' <<>> builder p <<>> builder time
    builder (MsgSubscribe p) = builder 'S' <<>> builder p
    builder (MsgUnsubscribe p) = builder 'U' <<>> builder p
    builder (MsgAssignType np tp) = builder 'T' <<>> builder np <<>> builder tp
    builder (MsgDelete p) = builder 'D' <<>> builder p
    builder (MsgChildren p ns) = builder 'C' <<>> builder p <<>> encodeListN ns

    parser = parser >>= parseByTag
      where
        parseByTag 'E' = MsgError <$> parser <*> parser
        parseByTag 's' =
            MsgSet <$> parser <*> parser <*> parser <*> parser <*> parser <*>
            parser
        parseByTag 'a' =
            MsgAdd <$> parser <*> parser <*> parser <*> parser <*> parser <*>
            parser
        parseByTag 'r' = MsgRemove <$> parser <*> parser <*> parser <*> parser
        parseByTag 'c' = MsgClear <$> parser <*> parser <*> parser <*> parser
        parseByTag 'S' = MsgSubscribe <$> parser
        parseByTag 'U' = MsgUnsubscribe <$> parser
        parseByTag 'T' = MsgAssignType <$> parser <*> parser
        parseByTag 'D' = MsgDelete <$> parser
        parseByTag 'C' = MsgChildren <$> parser <*> parser
        parseByTag c = badTag "message" c


buildInterpolation :: Interpolation -> Builder
buildInterpolation IConstant = fromChar 'C'
buildInterpolation ILinear = fromChar 'L'
buildInterpolation (IBezier a b) = fromChar 'B' <> fromWord32be a <> fromWord32be b


instance Serialisable Interpolation where
    builder = return . buildInterpolation
    parser = do
        c <- parser
        case c of
          'C' -> return IConstant
          'L' -> return ILinear
          'B' -> IBezier <$> parser <*> parser
          c -> badTag "interpolation" c


instance (Serialisable a) => Serialisable (Maybe a) where
    builder (Just a) = builder 'J' <<>> builder a
    builder Nothing = builder 'N'
    parser = do
        c <- parser
        case c of
          'J' -> Just <$> parser
          'N' -> return Nothing
          c -> badTag "maybe" c
