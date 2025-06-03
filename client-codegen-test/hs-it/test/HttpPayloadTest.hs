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
import Data.CaseInsensitive qualified as CI
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Message (State (..), defaultResponse)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit

testHttpPayload :: State -> HUnit.Test
testHttpPayload state = HUnit.TestCase $ do
  let identifierValue = 123
      stringHeaderValue = "test-header-value"
      prefixHeadersValue = Map.fromList [("custom", "prefix-value")]

      expectedMethod = HTTP.methodPost
      expectedPath = ["payload", T.pack $ show identifierValue]

      -- Expected JSON payload
      expectedPayloadJson = "{\"type\":\"LATTE\",\"description\":\"A smooth latte with silky foam\"}"

      coffeeItemResult = CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.LATTE
        CoffeeItem.setDescription "A smooth latte with silky foam"

  -- Set up response
  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  -- Create coffee item

  case coffeeItemResult of
    Left err ->
      HUnit.assertFailure ("Failed to create coffee item: " ++ T.unpack err)
    Right coffeeItem -> do
      -- Execute the operation
      result <- TestHttpPayload.testHttpPayload (client state) $ do
        TestHttpPayloadInput.setPayload coffeeItem
        TestHttpPayloadInput.setIdentifier identifierValue
        TestHttpPayloadInput.setStringheader (Just stringHeaderValue)
        TestHttpPayloadInput.setPrefixheaders (Just prefixHeadersValue)

      -- Take the request and compare it
      actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
      actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)

      -- Verify path is correct
      HUnit.assertEqual "Path should match" expectedPath (Wai.pathInfo actualReq)

      -- Verify HTTP method is correct
      HUnit.assertEqual "HTTP method should be POST" expectedMethod (Wai.requestMethod actualReq)

      -- Verify headers
      let actualHeaders =
            Wai.requestHeaders actualReq
              & map (\(n, v) -> (decodeUtf8 (CI.original n), v))

      HUnit.assertBool "String header should be present with correct value" $
        any (\(n, v) -> n == "x-header-string" && v == BS.pack (T.unpack stringHeaderValue)) actualHeaders

      HUnit.assertBool "Prefix header should be present with correct value" $
        any (\(n, v) -> n == "x-prefix-custom" && v == BS.pack (T.unpack $ prefixHeadersValue Map.! "custom")) actualHeaders

      -- Parse both as JSON and compare to handle potential formatting differences
      case (Aeson.decodeStrict actualPayload, Aeson.decodeStrict expectedPayloadJson) of
        (Just actualJson, Just expectedJson) -> do
          putStrLn $ "\nPAYLOAD_TEST :: Expected payload: " ++ show expectedJson
          putStrLn $ "\nPAYLOAD_TEST :: Actual payload: " ++ show actualJson
          HUnit.assertEqual "Payload JSON should match" (expectedJson :: Aeson.Value) actualJson
        _ -> HUnit.assertFailure "Failed to parse JSON payload"

      -- Verify operation result
      case result of
        Left (TestHttpPayload.BuilderError err) ->
          HUnit.assertFailure $ "Builder error: " ++ T.unpack err
        Left (TestHttpPayload.RequestError err) ->
          HUnit.assertFailure $ "Request error: " ++ T.unpack err
        Left (TestHttpPayload.InternalServerError _) ->
          HUnit.assertFailure "Unexpected server error"
        Right _ ->
          HUnit.assertBool "TestHttpPayload operation successful" True