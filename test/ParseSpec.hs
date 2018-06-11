{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module ParseSpec where

import Text.RawString.QQ

import SpecHelper

spec :: Spec
spec = do
  it "parses samples" $ do
    mapM_ (\x -> parse x `shouldSatisfy` isRight) [p1, p2, p3, p4, p5, p6]

main :: IO ()
main = hspec spec

p1 = [r|
{
  "payload": "YPkvASYgAAAemoR/",
  "message": {
    "app_id": "basetest",
    "dev_id": "tester",
    "port": 0
  },
  "gateway_id": "eui-b827ebfffe114355",
  "config": {
    "modulation": "LORA",
    "data_rate": "SF9BW125",
    "airtime": 144384000,
    "frequency": 869525000,
    "power": 27
  }
}
|]

p2 = [r|
{
  "app_id": "basetest",
  "dev_id": "tester",
  "hardware_serial": "0004A30B001E0D4D",
  "port": 1,
  "counter": 1276,
  "confirmed": true,
  "payload_raw": "jZOoQaCyMkI=",
  "metadata": {
    "time": "2018-02-09T19:10:19.912012664Z",
    "frequency": 867.3,
    "modulation": "LORA",
    "data_rate": "SF12BW125",
    "airtime": 1482752000,
    "coding_rate": "4/5",
    "gateways": [
      {
        "gtw_id": "eui-b827ebfffec6e5d0",
        "timestamp": 2377743228,
        "time": "2018-02-09T19:10:19.884395Z",
        "channel": 4,
        "rssi": -83,
        "snr": 8.5,
        "rf_chain": 0,
        "latitude": 49.19936,
        "longitude": 16.57975,
        "altitude": 320
      },
      {
        "gtw_id": "eui-1dee0933d840ee1b",
        "timestamp": 4138754052,
        "time": "2018-02-09T19:16:34.416079Z",
        "channel": 4,
        "rssi": -115,
        "snr": -16,
        "rf_chain": 0
      }
    ]
  }
}
|]

p3 = [r|
{
  "payload": "YO0qASYgmwblb5If",
  "message": {
    "app_id": "basetest",
    "dev_id": "tester",
    "port": 0
  },
  "gateway_id": "eui-b827ebfffec6e5d0",
  "config": {
    "modulation": "LORA",
    "data_rate": "SF9BW125",
    "airtime": 144384000,
    "counter": 1691,
    "frequency": 869525000,
    "power": 27
  }
}
|]

p4 = [r|
{
  "app_id": "basetest",
  "dev_id": "tester",
  "hardware_serial": "0004A30B001E0D4D",
  "port": 1,
  "counter": 1276,
  "confirmed": true,
  "payload_raw": "jZOoQaCyMkI=",
  "metadata": {
    "time": "2018-02-09T19:10:19.912012664Z",
    "frequency": 867.3,
    "modulation": "LORA",
    "data_rate": "SF12BW125",
    "airtime": 1482752000,
    "coding_rate": "4/5",
    "gateways": [
      {
        "gtw_id": "eui-b827ebfffec6e5d0",
        "timestamp": 2377743228,
        "time": "2018-02-09T19:10:19.884395Z",
        "channel": 4,
        "rssi": -83,
        "snr": 8.5,
        "rf_chain": 0,
        "latitude": 49.19936,
        "longitude": 16.57975,
        "altitude": 320
      },
      {
        "gtw_id": "eui-1dee0933d840ee1b",
        "timestamp": 4138754052,
        "time": "2018-02-09T19:16:34.416079Z",
        "channel": 4,
        "rssi": -115,
        "snr": -16,
        "rf_chain": 0
      }
    ]
  }
}
|]

p5 = [r|
{
  "app_id": "basetest",
  "dev_id": "tester",
  "hardware_serial": "0004A30B001E0D4D",
  "port": 1,
  "counter": 0,
  "confirmed": true,
  "is_retry": true,
  "payload_raw": "SA==",
  "metadata": {
    "time": "2017-10-27T20:31:23.636977619Z",
    "frequency": 867.5,
    "modulation": "LORA",
    "data_rate": "SF12BW125",
    "coding_rate": "4/5",
    "gateways": [
      {
        "gtw_id": "eui-b827ebfffec6e5d0",
        "timestamp": 2729574940,
        "time": "2017-10-27T20:31:23.3235Z",
        "channel": 5,
        "rssi": -112,
        "snr": -9.2,
        "rf_chain": 0,
        "latitude": 49.19936,
        "longitude": 16.57975,
        "altitude": 320
      }
    ]
  }
}
|]

p6 = [r|
{
  "app_id": "basetest",
  "dev_id": "tester",
  "hardware_serial": "0004A30B001E0D4D",
  "port": 1,
  "counter": 3,
  "confirmed": true,
  "payload_raw": "8mmxQVBmM0I=",
  "metadata": {
    "time": "2017-10-28T03:43:47.534601976Z",
    "frequency": 867.1,
    "modulation": "LORA",
    "data_rate": "SF12BW125",
    "coding_rate": "4/5",
    "gateways": [
      {
        "gtw_id": "eui-b827ebfffec6e5d0",
        "timestamp": 2903809348,
        "time": "2017-10-28T03:43:47.361855Z",
        "channel": 3,
        "rssi": -119,
        "snr": -10.5,
        "rf_chain": 0,
        "latitude": 49.19936,
        "longitude": 16.57975,
        "altitude": 320
      }
    ]
  }
}
|]
