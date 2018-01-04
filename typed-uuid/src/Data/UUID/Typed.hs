{-# LANGUAGE DeriveGeneric #-}

module Data.UUID.Typed where

import Control.Monad.IO.Class
import GHC.Generics
import Text.Read

import Control.DeepSeq
import Data.Aeson as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Text (Text)
import Web.HttpApiData

import Data.Validity
import Data.Validity.UUID ()

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

newtype UUID a = UUID
    { unUUID :: UUID.UUID
    } deriving (Eq, Generic)

instance Validity (UUID a)

instance Show (UUID a) where
    show (UUID u) = show u

instance Read (UUID a) where
    readPrec = UUID <$> readPrec

uuidBs :: UUID a -> SB.ByteString
uuidBs (UUID uuid) = UUID.toASCIIBytes uuid

uuidLBs :: UUID a -> LB.ByteString
uuidLBs = LB.fromStrict . uuidBs

uuidString :: UUID a -> String
uuidString (UUID uuid) = UUID.toString uuid

uuidText :: UUID a -> Text
uuidText (UUID uuid) = UUID.toText uuid

nextRandomUUID :: MonadIO m => m (UUID a)
nextRandomUUID = liftIO $ UUID <$> UUID.nextRandom

parseUUID :: Text -> Maybe (UUID a)
parseUUID = fmap UUID . UUID.fromText

parseUUIDString :: String -> Maybe (UUID a)
parseUUIDString = fmap UUID . UUID.fromString

instance NFData (UUID a)

instance FromJSONKey (UUID a) where
    fromJSONKey = FromJSONKeyTextParser textJSONParseUUID

instance ToJSONKey (UUID a) where
    toJSONKey = toJSONKeyText (UUID.toText . unUUID)

instance FromJSON (UUID a) where
    parseJSON = jsonParseUUID

jsonParseUUID :: Value -> Parser (UUID a)
jsonParseUUID = withText "PersonUuid" textJSONParseUUID

textJSONParseUUID :: Text -> Parser (UUID a)
textJSONParseUUID t =
    case UUID.fromText t of
        Nothing -> fail "Invalid Text when parsing UUID"
        Just u -> pure $ UUID u

instance ToJSON (UUID a) where
    toJSON (UUID u) = JSON.String $ UUID.toText u

instance FromHttpApiData (UUID a) where
    parseUrlPiece t =
        case UUID.fromText t of
            Nothing -> fail $ "Invalid UUID in Url Piece: " ++ T.unpack t
            Just uuid -> pure $ UUID uuid

instance ToHttpApiData (UUID a) where
    toUrlPiece (UUID uuid) = UUID.toText uuid
