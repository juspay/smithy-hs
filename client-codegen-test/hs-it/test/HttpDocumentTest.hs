{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpDocumentTest where

import Com.Example.Command.TestHttpDocument qualified as TestHttpDocument
import Com.Example.Model.CoffeeCustomization qualified as CoffeeCustomization
import Com.Example.Model.CoffeeItem qualified as CoffeeItem
import Com.Example.Model.CoffeeType qualified as CoffeeType
import Com.Example.Model.MilkType qualified as MilkType
import Com.Example.Model.TestHttpDocumentInput qualified as TestHttpDocumentInput
import Control.Concurrent.STM qualified as Stm
import Data.Aeson qualified as Aeson
import Data.Aeson qualified as Aeson.Value
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Message (State (..), defaultResponse)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit

testHttpDocument :: State -> HUnit.Test
testHttpDocument state = HUnit.TestCase $ do
  let identifierValue = 456
      stringHeaderValue = "test-document-header"
      prefixHeadersValue = Map.fromList [("doc", "document-prefix-value")]

      expectedMethod = HTTP.methodPost
      expectedPath = ["document", T.pack $ show identifierValue]

      expectedPayloadJson =
        "{\"payload\":{\"type\":\"ESPRESSO\",\"description\":\"Strong espresso shot\"},\"customization\":{\"milk\":\"OAT\"}}"

      coffeeItemResult = CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.ESPRESSO
        CoffeeItem.setDescription "Strong espresso shot"

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  case coffeeItemResult of
    Left err ->
      HUnit.assertFailure ("Failed to create coffee item: " ++ T.unpack err)
    Right coffeeItem -> do
      -- Execute the operation
      result <- TestHttpDocument.testHttpDocument (client state) $ do
        TestHttpDocumentInput.setPayload (Just coffeeItem)
        TestHttpDocumentInput.setCustomization (Just $ CoffeeCustomization.Milk MilkType.OAT)
        TestHttpDocumentInput.setIdentifier identifierValue
        TestHttpDocumentInput.setStringheader (Just stringHeaderValue)
        TestHttpDocumentInput.setPrefixheaders (Just prefixHeadersValue)

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
        any (\(n, v) -> n == "x-prefix-doc" && v == BS.pack (T.unpack $ prefixHeadersValue Map.! "doc")) actualHeaders

      case (Aeson.decodeStrict @Aeson.Value.Object actualPayload, Aeson.decodeStrict @Aeson.Value.Object expectedPayloadJson) of
        (Just actualJson, Just expectedJson) -> do
          putStrLn $ "\nDOCUMENT_TEST :: Expected document paload: " ++ show expectedPayloadJson
          putStrLn $ "\nDOCUMENT_TEST :: Actual document payload: " ++ show actualPayload

          -- Should contain both "payload" and "customization" fields
          HUnit.assertEqual "Payload should match" expectedJson actualJson
        _ -> HUnit.assertFailure "Failed to parse JSON document"

      -- Verify operation result
      case result of
        Left (TestHttpDocument.BuilderError err) ->
          HUnit.assertFailure $ "Builder error: " ++ T.unpack err
        Left (TestHttpDocument.RequestError err) ->
          HUnit.assertFailure $ "Request error: " ++ T.unpack err
        Left (TestHttpDocument.InternalServerError _) ->
          HUnit.assertFailure "Unexpected server error"
        Right output ->
          HUnit.assertBool "TestHttpDocument operation successful" True