{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpDocumentDeserializationTest where

import Com.Example.Command.TestHttpDocumentDeserialization qualified as TestHttpDocumentDeserialization
import Com.Example.Model.CoffeeCustomization qualified as CoffeeCustomization
import Com.Example.Model.CoffeeItem qualified as CoffeeItem
import Com.Example.Model.CoffeeType qualified as CoffeeType
import Com.Example.Model.TemperaturePreference qualified as TemperaturePreference
import Com.Example.Model.TestHttpDocumentDeserializationInput qualified as TestHttpDocumentDeserializationInput
import Com.Example.Model.TestHttpDocumentDeserializationOutput qualified as TestHttpDocumentDeserializationOutput
import Control.Concurrent.STM qualified as Stm
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Data.Function
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Message (State (..), RequestInternal (RequestInternal), assertEqRequest)
import qualified Message as RI
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit
import Network.HTTP.Date (parseHTTPDate)
import Data.Either.Extra (fromRight')
import Data.Maybe (fromJust)
import Data.Text.Encoding (decodeUtf8)

testHttpDocumentDeserialization :: State -> HUnit.Test
testHttpDocumentDeserialization state = HUnit.TestCase $ do
  let coffeeTypeValue = "POUR_OVER"
      dateString = "Fri, 05 Jan 2024 16:20:00 GMT"
      expectedTimeValue = fromJust $ parseHTTPDate dateString

      expectedOutputHeader = "test-document-output-header"
      expectedOutputHeaderInt = 789
      expectedOutputHeaderBool = False
      expectedOutputHeaderList = ["doc1", "doc2", "doc3"]
      expectedOutputPrefixHeaders = Map.fromList [("metadata", "doc-metadata"), ("version", "doc-v1")]

      expectedCoffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.POUR_OVER
        CoffeeItem.setDescription "Hand-poured coffee for document test"
        CoffeeItem.setCreatedat expectedTimeValue

      expectedCustomization = CoffeeCustomization.Temperature TemperaturePreference.HOT

      expectedRequest = RequestInternal {
        RI.queryString = [ ("type", Just coffeeTypeValue) ],
        RI.pathInfo = ["document_response"],
        RI.requestMethod = HTTP.methodGet,
        RI.requestHeaders = [("Authorization", "Bearer test-token")]
      }

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", BS.pack expectedOutputHeader),
            ("x-output-header-int", BS.pack $ show expectedOutputHeaderInt),
            ("x-output-header-bool", BS.pack $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", BS.pack $ T.unpack $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-prefix-metadata", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "metadata"),
            ("x-output-prefix-version", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "version"),
            ("x-output-header-time", dateString),
            ("Content-Type", "application/json")
          ]
          (Aeson.object
            [ "item" Aeson..= expectedCoffeeItem
            , "customization" Aeson..= expectedCustomization
            , "time" Aeson..= decodeUtf8 dateString
            ]
            & Aeson.encode)

      expectedOutput = fromRight' $ TestHttpDocumentDeserializationOutput.build $ do
        TestHttpDocumentDeserializationOutput.setOutputheader (Just $ T.pack expectedOutputHeader)
        TestHttpDocumentDeserializationOutput.setOutputheaderint (Just expectedOutputHeaderInt)
        TestHttpDocumentDeserializationOutput.setOutputheaderbool (Just expectedOutputHeaderBool)
        TestHttpDocumentDeserializationOutput.setOutputheaderlist (Just expectedOutputHeaderList)
        TestHttpDocumentDeserializationOutput.setOutputprefixheaders (Just expectedOutputPrefixHeaders)
        TestHttpDocumentDeserializationOutput.setTime (Just expectedTimeValue)
        TestHttpDocumentDeserializationOutput.setItem (Just expectedCoffeeItem)
        TestHttpDocumentDeserializationOutput.setCustomization (Just expectedCustomization)


  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestHttpDocumentDeserialization.testHttpDocumentDeserialization (client state) $ do
    TestHttpDocumentDeserializationInput.setCoffeetype (Just $ decodeUtf8 coffeeTypeValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedRequest actualReq

  case result of
    Left (TestHttpDocumentDeserialization.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpDocumentDeserialization.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpDocumentDeserialization.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output
