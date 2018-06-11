{-# LANGUAGE OverloadedStrings #-}

module DownlinkSpec where

import SpecHelper
import Data.Aeson

spec :: Spec
spec = do
  it "parses downlink" $ do
    let d = Downlink {
              downlinkAppId = "APP"
            , downlinkDevId = "DEV"
            , downlinkPort  = 0
            , downlinkPayloadRaw = "payload"
            , downlinkConfirmed = False
            , downlinkSchedule = Just ScheduleFirst
            }

    (decode . encode $ d) `shouldBe` (Just d)

main :: IO ()
main = hspec spec
