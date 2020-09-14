module Distillery.Extractor
where

import Distillery.Prelude hiding (lookup)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as AttoparsecText
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
import qualified AesonValueParser


{-|
An abstraction over a partial function,
which generates a textual information when the input value is out of its domain.

This abstraction is incredibly universal,
covering all cases where you need to refine or parse a value.
It is also incredibly composable, providing all kinds of instances.
-}
newtype Extractor a b =
  Extractor (a -> Either Text b)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus)
    via (ExceptT Text ((->) a))
  deriving (Profunctor, Strong, Cochoice, Choice, Traversing, Category)
    via (Star (Either Text))

deriving via (Either Text) instance Sieve Extractor (Either Text)


extract :: Extractor a b -> a -> Either Text b
extract =
  coerce

lookupInHashMap :: (Hashable a, Eq a) => a -> Extractor (HashMap a b) b
lookupInHashMap key =
  Extractor $ \ map ->
    case HashMap.lookup key map of
      Just value -> Right value
      Nothing -> Left "Key not found"

dictionary :: (Hashable a, Eq a, Show a) => Text -> [(a, b)] -> Extractor a b
dictionary label mappingList =
  let
    expectedValuesText =
      fromString (show (fmap fst mappingList))
    mappingListLength =
      length mappingList
    !narrow =
      if mappingListLength > 512
        then let
          !hashMap =
            HashMap.fromList mappingList
          in flip HashMap.lookup hashMap
        else flip List.lookup mappingList
    extract a =
      case narrow a of
        Just b -> Right b
        _ -> Left ("Unexpected " <> label <> ": \"" <> showAsText a <> "\". Expecting one of: " <> expectedValuesText)
    in Extractor extract

attoparseText :: AttoparsecText.Parser a -> Extractor Text a
attoparseText parser =
  Extractor (first fromString . AttoparsecText.parseOnly parser)

parseAesonValue :: AesonValueParser.Value a -> Extractor Aeson.Value a
parseAesonValue parser =
  Extractor (AesonValueParser.runWithTextError parser)

decodeUtf8 :: Extractor ByteString Text
decodeUtf8 =
  Extractor (first (fromString . show) . Text.decodeUtf8')
