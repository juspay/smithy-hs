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
import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Message (State (..), defaultResponse, RequestInternal (RequestInternal), assertEqRequest)
import qualified Message as RI
import Network.HTTP.Types qualified as HTTP
import Test.HUnit qualified as HUnit
import Data.Either.Extra (fromRight')
import Data.Maybe (fromJust)
import Network.HTTP.Date (parseHTTPDate)

testHttpDocument :: State -> HUnit.Test
testHttpDocument state = HUnit.TestCase $ do
  let dateString = "Thu, 04 Jan 2024 14:45:00 GMT"
      identifierValue = 456
      stringHeaderValue = "test-document-header"
      prefixHeadersValue = Map.fromList [("doc", "document-prefix-value")]
      time = fromJust $ parseHTTPDate dateString

      coffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.ESPRESSO
        CoffeeItem.setDescription "Strong espresso shot"
        CoffeeItem.setCreatedat time
      customization = CoffeeCustomization.Milk MilkType.OAT

      expectedPayloadJson = Aeson.object
        [ "payload" Aeson..= coffeeItem
        , "customization" Aeson..= customization
        , "time" Aeson..= decodeUtf8 dateString
        ]

      expectedRequest = RequestInternal {
        RI.queryString = [],
        RI.pathInfo = ["document", T.pack $ show identifierValue],
        RI.requestMethod = HTTP.methodPost,
        RI.requestHeaders = [
          ("x-header-string", BS.pack $ T.unpack stringHeaderValue),
          ("x-prefix-doc", BS.pack $ T.unpack $ prefixHeadersValue Map.! "doc"),
          ("Authorization", "Bearer test-token")
        ]
      }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpDocument.testHttpDocument (client state) $ do
    TestHttpDocumentInput.setPayload (Just coffeeItem)
    TestHttpDocumentInput.setCustomization (Just customization)
    TestHttpDocumentInput.setTime (Just time)
    TestHttpDocumentInput.setIdentifier identifierValue
    TestHttpDocumentInput.setStringheader (Just stringHeaderValue)
    TestHttpDocumentInput.setPrefixheaders (Just prefixHeadersValue)

  -- Take the request and compare it
  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)

  assertEqRequest expectedRequest actualReq
  case Aeson.decodeStrict actualPayload of
    Just actualJson -> do
      HUnit.assertEqual "Payload should match" expectedPayloadJson actualJson
    _ -> HUnit.assertFailure "Failed to parse JSON document"

  -- Verify operation result
  case result of
    Left (TestHttpDocument.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpDocument.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpDocument.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right _ ->
      HUnit.assertBool "TestHttpDocument operation successful" True
