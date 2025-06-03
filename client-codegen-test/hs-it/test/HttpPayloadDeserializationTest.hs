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
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LC
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Message (State (..))
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.URI (parseQuery, queryToQueryText)
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit

testHttpPayloadDeserialization :: State -> HUnit.Test
testHttpPayloadDeserialization state = HUnit.TestCase $ do
  let coffeeTypeValue = "ESPRESSO"

      expectedMethod = HTTP.methodGet
      expectedPath = ["payload_response"]

      expectedQueryItems =
        [ ("type", Just coffeeTypeValue)
        ]

      expectedOutputHeader = "test-output-header"
      expectedOutputHeaderInt = 123
      expectedOutputHeaderBool = True
      expectedOutputHeaderList = ["item1", "item2", "item3"]
      expectedOutputPrefixHeaders = Map.fromList [("custom", "prefix-value"), ("another", "another-value")]
      expectedCoffeeItemJson = "{\"type\":\"LATTE\",\"description\":\"Test latte for deserialization\"}"
      exptecdCoffeeItem =
        fromJust $ Aeson.decode @CoffeeItem.CoffeeItem (LBS.fromStrict $ BS.pack expectedCoffeeItemJson)

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", BS.pack expectedOutputHeader),
            ("x-output-header-int", BS.pack $ show expectedOutputHeaderInt),
            ("x-output-header-bool", BS.pack $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", BS.pack $ T.unpack $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-prefix-custom", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "custom"),
            ("x-output-prefix-another", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "another"),
            ("Content-Type", "application/json")
          ]
          (LC.pack expectedCoffeeItemJson)

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestHttpPayloadDeserialization.testHttpPayloadDeserialization (client state) $ do
    TestHttpPayloadDeserializationInput.setCoffeetype (Just coffeeTypeValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  HUnit.assertEqual "Path should match" expectedPath (Wai.pathInfo actualReq)

  HUnit.assertEqual "HTTP method should be GET" expectedMethod (Wai.requestMethod actualReq)

  let queryBytes = Wai.rawQueryString actualReq
      actualQueryItems = queryToQueryText $ parseQuery queryBytes

  putStrLn $ "Actual query string: " ++ show queryBytes
  putStrLn $ "Parsed query items: " ++ show actualQueryItems

  HUnit.assertBool "All expected query parameters should be present" $
    all (\expectedItem -> expectedItem `elem` actualQueryItems) expectedQueryItems

  case result of
    Left (TestHttpPayloadDeserialization.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpPayloadDeserialization.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpPayloadDeserialization.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right output -> do
      putStrLn $ "Expected output header: " ++ expectedOutputHeader
      putStrLn $ "Expected output header int: " ++ show expectedOutputHeaderInt
      putStrLn $ "Expected output header bool: " ++ show expectedOutputHeaderBool
      putStrLn $ "Expected output header list: " ++ show expectedOutputHeaderList
      putStrLn $ "Expected output prefix headers: " ++ show expectedOutputPrefixHeaders

      putStrLn $ "Output header: " ++ show (TestHttpPayloadDeserializationOutput.outputHeader output)
      putStrLn $ "Output header int: " ++ show (TestHttpPayloadDeserializationOutput.outputHeaderInt output)
      putStrLn $ "Output header bool: " ++ show (TestHttpPayloadDeserializationOutput.outputHeaderBool output)
      putStrLn $ "Output header list: " ++ show (TestHttpPayloadDeserializationOutput.outputHeaderList output)
      putStrLn $ "Output prefix headers: " ++ show (TestHttpPayloadDeserializationOutput.outputPrefixHeaders output)

      HUnit.assertEqual
        "Output header should match"
        (Just $ T.pack expectedOutputHeader)
        (TestHttpPayloadDeserializationOutput.outputHeader output)

      HUnit.assertEqual
        "Output header int should match"
        (Just expectedOutputHeaderInt)
        (TestHttpPayloadDeserializationOutput.outputHeaderInt output)

      HUnit.assertEqual
        "Output header bool should match"
        (Just expectedOutputHeaderBool)
        (TestHttpPayloadDeserializationOutput.outputHeaderBool output)

      HUnit.assertEqual
        "Output header list should match"
        (Just expectedOutputHeaderList)
        (TestHttpPayloadDeserializationOutput.outputHeaderList output)

      HUnit.assertEqual
        "Output prefix headers should match"
        (Just expectedOutputPrefixHeaders)
        (TestHttpPayloadDeserializationOutput.outputPrefixHeaders output)

      case TestHttpPayloadDeserializationOutput.item output of
        Nothing -> HUnit.assertFailure "Coffee item should be present in payload"
        Just coffeeItem ->
          do
            putStrLn $ "Expected coffee item: " ++ show exptecdCoffeeItem
            putStrLn $ "Actual Coffee item: " ++ show coffeeItem
            HUnit.assertEqual
              "Coffee item type should match"
              CoffeeType.LATTE
              (CoffeeItem.coffeeType coffeeItem)
            HUnit.assertEqual
              "Coffee item description should match"
              "Test latte for deserialization"
              (CoffeeItem.description coffeeItem)