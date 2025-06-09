{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpPayloadDeserializationTest where

import Com.Example.Command.TestHttpPayloadDeserialization qualified as TestHttpPayloadDeserialization
import Com.Example.Model.CoffeeItem qualified as CoffeeItem
import Com.Example.Model.CoffeeType qualified as CoffeeType
import Com.Example.Model.TestHttpPayloadDeserializationInput qualified as TestHttpPayloadDeserializationInput
import Com.Example.Model.TestHttpPayloadDeserializationOutput qualified as TestHttpPayloadDeserializationOutput
import Control.Concurrent.STM qualified as Stm
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Message (State (..), assertEqRequest, RequestInternal (RequestInternal))
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit
import Data.Aeson (encode)
import Network.HTTP.Date (parseHTTPDate)
import Data.Either.Extra (fromRight')
import Data.Maybe (fromJust)
import Data.Text.Encoding (decodeUtf8)
import qualified Message as RI

testHttpPayloadDeserialization :: State -> HUnit.Test
testHttpPayloadDeserialization state = HUnit.TestCase $ do
  let coffeeTypeValue = "ESPRESSO"
      dateString = "Fri, 05 Jan 2024 16:20:00 GMT"

      expectedOutputHeader = "test-output-header"
      expectedOutputHeaderInt = 123
      expectedOutputHeaderBool = True
      expectedOutputHeaderList = ["item1", "item2", "item3"]
      expectedOutputPrefixHeaders = Map.fromList [("custom", "prefix-value"), ("another", "another-value")]
      expectedTimeValue = fromJust $ parseHTTPDate dateString
      expectedCoffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.LATTE
        CoffeeItem.setDescription "Test latte for deserialization"
        CoffeeItem.setCreatedat expectedTimeValue

      expectedOutput = fromRight' $ TestHttpPayloadDeserializationOutput.build $ do
        TestHttpPayloadDeserializationOutput.setOutputheader (Just $ T.pack expectedOutputHeader)
        TestHttpPayloadDeserializationOutput.setOutputheaderint (Just expectedOutputHeaderInt)
        TestHttpPayloadDeserializationOutput.setOutputheaderbool (Just expectedOutputHeaderBool)
        TestHttpPayloadDeserializationOutput.setOutputheaderlist (Just expectedOutputHeaderList)
        TestHttpPayloadDeserializationOutput.setTime (Just expectedTimeValue)
        TestHttpPayloadDeserializationOutput.setOutputprefixheaders (Just expectedOutputPrefixHeaders)
        TestHttpPayloadDeserializationOutput.setItem (Just expectedCoffeeItem)

      expectedReq = RequestInternal {
          RI.requestMethod = HTTP.methodGet,
          RI.pathInfo = ["payload_response"],
          RI.queryString = [ ("type", Just coffeeTypeValue) ],
          RI.requestHeaders = [
            ("Authorization", "Bearer test-token")
          ]
        }

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", BS.pack expectedOutputHeader),
            ("x-output-header-int", BS.pack $ show expectedOutputHeaderInt),
            ("x-output-header-bool", BS.pack $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", BS.pack $ T.unpack $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-header-time", dateString),
            ("x-output-prefix-custom", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "custom"),
            ("x-output-prefix-another", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "another"),
            ("Content-Type", "application/json")
          ]
          (encode expectedCoffeeItem)

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestHttpPayloadDeserialization.testHttpPayloadDeserialization (client state) $ do
    TestHttpPayloadDeserializationInput.setCoffeetype (Just $ decodeUtf8 coffeeTypeValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedReq actualReq
  case result of
    Left (TestHttpPayloadDeserialization.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpPayloadDeserialization.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpPayloadDeserialization.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output
