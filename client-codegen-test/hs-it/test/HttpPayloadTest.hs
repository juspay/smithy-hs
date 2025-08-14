{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpPayloadTest where

import Com.Example.Command.TestHttpPayload qualified as TestHttpPayload
import Com.Example.Model.CoffeeItem qualified as CoffeeItem
import Com.Example.Model.CoffeeType qualified as CoffeeType
import Com.Example.Model.TestHttpPayloadInput qualified as TestHttpPayloadInput
import Control.Concurrent.STM qualified as Stm
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Message (State (..), defaultResponse, assertEqRequest, RequestInternal (RequestInternal))
import qualified Message as RI
import Network.HTTP.Types qualified as HTTP
import Test.HUnit qualified as HUnit
import Data.Either.Extra (fromRight')
import Data.Maybe (fromJust)
import Network.HTTP.Date (parseHTTPDate)

testHttpPayload :: State -> HUnit.Test
testHttpPayload state = HUnit.TestCase $ do
  let dateString = "Thu, 04 Jan 2024 14:45:00 GMT"
      identifierValue = 123
      stringHeaderValue = "test-header-value"
      prefixHeadersValue = Map.fromList [("custom", "prefix-value")]
      time = fromJust $ parseHTTPDate dateString

      coffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.LATTE
        CoffeeItem.setDescription "A smooth latte with silky foam"
        CoffeeItem.setCreatedat time

      expectedPayload = Aeson.toJSON coffeeItem
      expectedRequest = RequestInternal {
        RI.queryString = [],
        RI.pathInfo = ["payload", T.pack $ show identifierValue],
        RI.requestMethod = HTTP.methodPost,
        RI.requestHeaders = [
          ("x-header-string", BS.pack $ T.unpack stringHeaderValue),
          ("x-prefix-custom", BS.pack $ T.unpack $ prefixHeadersValue Map.! "custom"),
          ("content-type", "application/json"),
          ("Authorization", "Bearer test-token")
        ]
      }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpPayload.testHttpPayload (client state) $ do
    TestHttpPayloadInput.setPayload coffeeItem
    TestHttpPayloadInput.setIdentifier identifierValue
    TestHttpPayloadInput.setStringheader (Just stringHeaderValue)
    TestHttpPayloadInput.setPrefixheaders (Just prefixHeadersValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)

  assertEqRequest expectedRequest actualReq

  case Aeson.decodeStrict actualPayload of
    Just actualJson -> do
      HUnit.assertEqual "Payload JSON should match" expectedPayload actualJson
    _ -> HUnit.assertFailure "Failed to parse JSON payload"

  case result of
    Left (TestHttpPayload.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpPayload.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpPayload.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right _ ->
      HUnit.assertBool "TestHttpPayload operation successful" True
