{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}

module Data.TTN.Types where

import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr, hPutStrLn)
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import Control.Monad (forM_, mzero, join)
import Control.Applicative
import Data.Aeson(decodeStrict, eitherDecodeStrict,
                  Value(..), FromJSON(..), ToJSON(..),
                  pairs, withText, (.:), (.:?), (.=), object)
import Data.Monoid
import Data.Text (Text, unpack)
import GHC.Generics

import Data.Time.RFC3339
import Data.Time.LocalTime

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)

data TTNZonedTime = TTNZonedTime { unwrap :: ZonedTime } deriving (Show, Generic)

instance ToJSON TTNZonedTime where
  toJSON     = toJSON . (formatTimeRFC3339 :: ZonedTime -> Text) . unwrap
  toEncoding =  toEncoding . (formatTimeRFC3339 :: ZonedTime -> Text) . unwrap

instance FromJSON TTNZonedTime where
  parseJSON = withText "TTNZonedTime" $ \x ->
    case parseTimeRFC3339 x of
      Nothing -> fail $ "Parsing RFC3339 value failed, got:" ++ show x
      Just t -> return $ TTNZonedTime t


-- https://github.com/TheThingsNetwork/ttn/blob/develop/core/types/event.go#L65
data Config = Config {
    configFrequency  :: Maybe Double
  , configDataRate   :: Maybe Text
  , configCounter    :: Maybe Double
  , configAirtime    :: Maybe Double
  , configPower      :: Maybe Double
  , configModulation :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .:?? "frequency"
    <*> v .:?? "data_rate"
    <*> v .:?? "counter"
    <*> v .:?? "airtime"
    <*> v .:?? "power"
    <*> v .:?? "modulation"
  parseJSON _          = mzero

instance ToJSON Config where
  toJSON     (Config {..}) = object [
      "frequency"   .= configFrequency
    , "data_rate"   .= configDataRate
    , "counter"     .= configCounter
    , "airtime"     .= configAirtime
    , "power"       .= configPower
    , "modulation"  .= configModulation
    ]
  toEncoding (Config {..}) = pairs (
       "frequency"  .= configFrequency
    <> "data_rate"  .= configDataRate
    <> "counter"    .= configCounter
    <> "airtime"    .= configAirtime
    <> "power"      .= configPower
    <> "modulation" .= configModulation
    )


-- https://github.com/TheThingsNetwork/ttn/blob/develop/core/types/gateway_metadata.go
-- https://github.com/TheThingsNetwork/ttn/blob/develop/core/types/location_metadata.go
data GatewaysElt = GatewaysElt {
    gatewaysEltGtwId                  :: Maybe Text
  , gatewaysEltGtwTrusted             :: Maybe Bool
  , gatewaysEltTimestamp              :: Maybe Int
  , gatewaysEltFineTimestamp          :: Maybe Int
  , gatewaysEltFineTimestampEncrypted :: Maybe Text
  , gatewaysEltTime                   :: Maybe TTNZonedTime
  , gatewaysEltAntenna                :: Maybe Text
  , gatewaysEltChannel                :: Double
  , gatewaysEltRSSI                   :: Double
  , gatewaysEltSNR                    :: Double
  , gatewaysEltRFChain                :: Int
  , gatewaysEltLatitude               :: Maybe Double
  , gatewaysEltLongitude              :: Maybe Double
  , gatewaysEltAltitude               :: Maybe Int
  , gatewaysEltAccuracy               :: Maybe Int
  , gatewaysEltSource                 :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON GatewaysElt where
  parseJSON (Object v) = GatewaysElt
    <$> v .:?? "gtw_id"
    <*> v .:?? "gtw_trusted"
    <*> v .:?? "timestamp"
    <*> v .:?? "fine_timestamp"
    <*> v .:?? "fine_timestamp_encrypted"
    <*> v .:?? "time"
    <*> v .:?? "antenna"
    <*> v .:   "channel"
    <*> v .:   "rssi"
    <*> v .:   "snr"
    <*> v .:   "rf_chain"
    <*> v .:?? "latitude"
    <*> v .:?? "longitude"
    <*> v .:?? "altitude"
    <*> v .:?? "accuracy"
    <*> v .:?? "source"
  parseJSON _          = mzero

instance ToJSON GatewaysElt where
  toJSON     (GatewaysElt {..}) = object [
      "gtw_id"                    .= gatewaysEltGtwId
    , "gtw_trusted"               .= gatewaysEltGtwTrusted
    , "timestamp"                 .= gatewaysEltTimestamp
    , "fine_timestamp"            .= gatewaysEltFineTimestamp
    , "fine_timestamp_encrypted"  .= gatewaysEltFineTimestampEncrypted
    , "time"                      .= gatewaysEltTime
    , "antenna"                   .= gatewaysEltAntenna
    , "channel"                   .= gatewaysEltChannel
    , "rssi"                      .= gatewaysEltRSSI
    , "snr"                       .= gatewaysEltSNR
    , "rf_chain"                  .= gatewaysEltRFChain
    , "latitude"                  .= gatewaysEltLatitude
    , "longitude"                 .= gatewaysEltLongitude
    , "altitude"                  .= gatewaysEltAltitude
    , "accuracy"                  .= gatewaysEltAccuracy
    , "source"                    .= gatewaysEltSource
    ]
  toEncoding (GatewaysElt {..}) = pairs (
       "gtw_id"                   .= gatewaysEltGtwId
    <> "gtw_trusted"              .= gatewaysEltGtwTrusted
    <> "timestamp"                .= gatewaysEltTimestamp
    <> "fine_timestamp"           .= gatewaysEltFineTimestamp
    <> "fine_timestamp_encrypted" .= gatewaysEltFineTimestampEncrypted
    <> "time"                     .= gatewaysEltTime
    <> "antenna"                  .= gatewaysEltAntenna
    <> "channel"                  .= gatewaysEltChannel
    <> "rssi"                     .= gatewaysEltRSSI
    <> "snr"                      .= gatewaysEltSNR
    <> "rf_chain"                 .= gatewaysEltRFChain
    <> "latitude"                 .= gatewaysEltLatitude
    <> "longitude"                .= gatewaysEltLongitude
    <> "altitude"                 .= gatewaysEltAltitude
    <> "accuracy"                 .= gatewaysEltAccuracy
    <> "source"                   .= gatewaysEltSource
    )


-- https://github.com/TheThingsNetwork/ttn/blob/develop/core/types/metadata.go
data Metadata = Metadata {
    metadataTime       :: Maybe TTNZonedTime
  , metadataFrequency  :: Maybe Double
  , metadataModulation :: Maybe Text
  , metadataDataRate   :: Maybe Text
  , metadataBitRate    :: Maybe Int
  , metadataAirtime    :: Maybe Double
  , metadataCodingRate :: Maybe Text
  , metadataGateways   :: [GatewaysElt]
  } deriving (Show, Generic)

instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$>
        v .:?? "time"
    <*> v .:?? "frequency"
    <*> v .:?? "modulation"
    <*> v .:?? "data_rate"
    <*> v .:?? "bit_rate"
    <*> v .:?? "airtime"
    <*> v .:?? "coding_rate"
    <*> v .:   "gateways"
  parseJSON _          = mzero

instance ToJSON Metadata where
  toJSON     (Metadata {..}) = object [
      "time"         .= metadataTime
    , "frequency"    .= metadataFrequency
    , "modulation"   .= metadataModulation
    , "data_rate"    .= metadataDataRate
    , "bit_rate"     .= metadataBitRate
    , "coding_rate"  .= metadataCodingRate
    , "airtime"      .= metadataAirtime
    , "gateways"     .= metadataGateways
    ]
  toEncoding (Metadata {..}) = pairs (
       "time" .= metadataTime
    <> "frequency"   .= metadataFrequency
    <> "modulation"  .= metadataModulation
    <> "data_rate"   .= metadataDataRate
    <> "bit_rate"    .= metadataBitRate
    <> "coding_rate" .= metadataCodingRate
    <> "airtime"     .= metadataAirtime
    <> "gateways"    .= metadataGateways
    )


data Message = Message {
    messageDevId :: Text,
    messageAppId :: Text,
    messagePort  :: Double
  } deriving (Show, Generic)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$> v .: "dev_id"
    <*> v .: "app_id"
    <*> v .: "port"
  parseJSON _          = mzero

instance ToJSON Message where
  toJSON     (Message {..}) = object [
      "dev_id"  .= messageDevId
    , "app_id"  .= messageAppId
    , "port"    .= messagePort
    ]
  toEncoding (Message {..}) = pairs (
       "dev_id" .= messageDevId
    <> "app_id" .= messageAppId
    <> "port"   .= messagePort
    )


-- https://github.com/TheThingsNetwork/ttn/blob/develop/core/types/uplink_message.go
data Uplink = Uplink {
    uplinkConfig         :: Maybe Config
  , uplinkGatewayId      :: Maybe Text
  , uplinkDevId          :: Maybe Text
  , uplinkPayload        :: Maybe Text
  , uplinkCounter        :: Maybe Double
  , uplinkIsRetry        :: Maybe Bool
  , uplinkMetadata       :: Maybe Metadata
  , uplinkPayloadRaw     :: Maybe Text
  , uplinkMessage        :: Maybe Message
  , uplinkAppId          :: Maybe Text
  , uplinkConfirmed      :: Maybe Bool
  , uplinkHardwareSerial :: Maybe Text
  , uplinkPort           :: Maybe Double
  } deriving (Show, Generic)

instance FromJSON Uplink where
  parseJSON (Object v) = Uplink
    <$> v .:?? "config"
    <*> v .:?? "gateway_id"
    <*> v .:?? "dev_id"
    <*> v .:?? "payload"
    <*> v .:?? "counter"
    <*> v .:?? "is_retry"
    <*> v .:?? "metadata"
    <*> v .:?? "payload_raw"
    <*> v .:?? "message"
    <*> v .:?? "app_id"
    <*> v .:?? "confirmed"
    <*> v .:?? "hardware_serial"
    <*> v .:?? "port"
  parseJSON _          = mzero

instance ToJSON Uplink where
  toJSON     (Uplink {..}) = object [
      "config"           .= uplinkConfig
    , "gateway_id"       .= uplinkGatewayId
    , "dev_id"           .= uplinkDevId
    , "payload"          .= uplinkPayload
    , "counter"          .= uplinkCounter
    , "is_retry"         .= uplinkIsRetry
    , "metadata"         .= uplinkMetadata
    , "payload_raw"      .= uplinkPayloadRaw
    , "message"          .= uplinkMessage
    , "app_id"           .= uplinkAppId
    , "confirmed"        .= uplinkConfirmed
    , "hardware_serial"  .= uplinkHardwareSerial
    , "port"             .= uplinkPort
    ]
  toEncoding (Uplink {..}) = pairs (
       "config"          .= uplinkConfig
    <> "gateway_id"      .= uplinkGatewayId
    <> "dev_id"          .= uplinkDevId
    <> "payload"         .= uplinkPayload
    <> "counter"         .= uplinkCounter
    <> "is_retry"        .= uplinkIsRetry
    <> "metadata"        .= uplinkMetadata
    <> "payload_raw"     .= uplinkPayloadRaw
    <> "message"         .= uplinkMessage
    <> "app_id"          .= uplinkAppId
    <> "confirmed"       .= uplinkConfirmed
    <> "hardware_serial" .= uplinkHardwareSerial
    <> "port"            .= uplinkPort
    )

data Schedule = ScheduleReplace | ScheduleFirst | ScheduleLast
  deriving (Eq, Show)

instance FromJSON Schedule where
  parseJSON = withText "Schedule" $ \x ->
    case x of
      "replace" -> return ScheduleReplace
      "first"   -> return ScheduleFirst
      "last"    -> return ScheduleLast
      w         -> fail $ "Unknown schedule" ++ (unpack w)

instance ToJSON Schedule where
  toJSON ScheduleReplace = "replace"
  toJSON ScheduleFirst   = "first"
  toJSON ScheduleLast    = "last"

-- https://github.com/TheThingsNetwork/ttn/blob/develop/core/types/downlink_message.go
data Downlink = Downlink {
    downlinkAppId          :: Text
  , downlinkDevId          :: Text
  , downlinkPort           :: Double
  , downlinkPayloadRaw     :: Text
  , downlinkConfirmed      :: Bool
  , downlinkSchedule       :: Maybe Schedule
  } deriving (Show, Eq, Generic)

instance FromJSON Downlink where
  parseJSON (Object v) = Downlink
    <$> v .:   "app_id"
    <*> v .:   "dev_id"
    <*> v .:   "port"
    <*> v .:   "payload_raw"
    <*> v .:   "confirmed"
    <*> v .:?? "schedule"
  parseJSON _          = mzero

instance ToJSON Downlink where
  toJSON     (Downlink {..}) = object [
      "app_id"           .= downlinkAppId
    , "dev_id"           .= downlinkDevId
    , "port"             .= downlinkPort
    , "payload_raw"      .= downlinkPayloadRaw
    , "confirmed"        .= downlinkConfirmed
    , "schedule"         .= downlinkSchedule
    ]
  toEncoding (Downlink {..}) = pairs (
       "app_id"          .= downlinkAppId
    <> "dev_id"          .= downlinkDevId
    <> "port"            .= downlinkPort
    <> "payload_raw"     .= downlinkPayloadRaw
    <> "confirmed"       .= downlinkConfirmed
    <> "schedule"        .= downlinkSchedule
    )

parse :: B.ByteString -> Either String Uplink
parse = eitherDecodeStrict

parseFile :: FilePath -> IO Uplink
parseFile filename = do
  input <- B.readFile filename
  case decodeStrict input of
    Nothing -> fatal $ case (decodeStrict input :: Maybe Value) of
                         Nothing -> "Invalid JSON file: "     ++ filename
                         Just v  -> "Mismatched JSON value from file: " ++ filename
    Just r  -> return (r :: Uplink)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

parseMany :: [String] -> IO ()
parseMany filenames = do
  forM_ filenames (\f -> parseFile f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess
