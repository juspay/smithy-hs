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
import Message (State (..))
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.URI (parseQuery, queryToQueryText)
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit

testHttpDocumentDeserialization :: State -> HUnit.Test
testHttpDocumentDeserialization state = HUnit.TestCase $ do
  let coffeeTypeValue = "POUR_OVER"

      expectedMethod = HTTP.methodGet
      expectedPath = ["document_response"]

      expectedQueryItems =
        [ ("type", Just coffeeTypeValue)
        ]

      expectedOutputHeader = "test-document-output-header"
      expectedOutputHeaderInt = 789
      expectedOutputHeaderBool = False
      expectedOutputHeaderList = ["doc1", "doc2", "doc3"]
      expectedOutputPrefixHeaders = Map.fromList [("metadata", "doc-metadata"), ("version", "doc-v1")]

      expectedDocumentJson =
        Aeson.object
          [ "item"
              Aeson..= Aeson.object
                [ "type" Aeson..= CoffeeType.POUR_OVER,
                  "description" Aeson..= Aeson.String "Hand-poured coffee for document test"
                ],
            "customization"
              Aeson..= Aeson.object
                [ "temperature" Aeson..= TemperaturePreference.HOT
                ]
          ]
          & Aeson.encode

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", BS.pack expectedOutputHeader),
            ("x-output-header-int", BS.pack $ show expectedOutputHeaderInt),
            ("x-output-header-bool", BS.pack $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", BS.pack $ T.unpack $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-prefix-metadata", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "metadata"),
            ("x-output-prefix-version", BS.pack $ T.unpack $ expectedOutputPrefixHeaders Map.! "version"),
            ("Content-Type", "application/json")
          ]
          expectedDocumentJson

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestHttpDocumentDeserialization.testHttpDocumentDeserialization (client state) $ do
    TestHttpDocumentDeserializationInput.setCoffeetype (Just coffeeTypeValue)

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
    Left (TestHttpDocumentDeserialization.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpDocumentDeserialization.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpDocumentDeserialization.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right output -> do
      putStrLn "=== HEADER VALIDATION ==="
      putStrLn $ "Expected outputHeader: " ++ show (Just $ T.pack expectedOutputHeader)
      putStrLn $ "Actual outputHeader: " ++ show (TestHttpDocumentDeserializationOutput.outputHeader output)

      putStrLn $ "Expected outputHeaderInt: " ++ show (Just expectedOutputHeaderInt)
      putStrLn $ "Actual outputHeaderInt: " ++ show (TestHttpDocumentDeserializationOutput.outputHeaderInt output)

      putStrLn $ "Expected outputHeaderBool: " ++ show (Just expectedOutputHeaderBool)
      putStrLn $ "Actual outputHeaderBool: " ++ show (TestHttpDocumentDeserializationOutput.outputHeaderBool output)

      putStrLn $ "Expected outputHeaderList: " ++ show (Just expectedOutputHeaderList)
      putStrLn $ "Actual outputHeaderList: " ++ show (TestHttpDocumentDeserializationOutput.outputHeaderList output)

      putStrLn $ "Expected outputPrefixHeaders: " ++ show (Just expectedOutputPrefixHeaders)
      putStrLn $ "Actual outputPrefixHeaders: " ++ show (TestHttpDocumentDeserializationOutput.outputPrefixHeaders output)

      HUnit.assertEqual
        "Output header should match"
        (Just $ T.pack expectedOutputHeader)
        (TestHttpDocumentDeserializationOutput.outputHeader output)

      HUnit.assertEqual
        "Output header int should match"
        (Just expectedOutputHeaderInt)
        (TestHttpDocumentDeserializationOutput.outputHeaderInt output)

      HUnit.assertEqual
        "Output header bool should match"
        (Just expectedOutputHeaderBool)
        (TestHttpDocumentDeserializationOutput.outputHeaderBool output)

      HUnit.assertEqual
        "Output header list should match"
        (Just expectedOutputHeaderList)
        (TestHttpDocumentDeserializationOutput.outputHeaderList output)

      HUnit.assertEqual
        "Output prefix headers should match"
        (Just expectedOutputPrefixHeaders)
        (TestHttpDocumentDeserializationOutput.outputPrefixHeaders output)

      putStrLn "=== DOCUMENT FIELD VALIDATION ==="

      case TestHttpDocumentDeserializationOutput.item output of
        Nothing -> do
          putStrLn "Expected item: CoffeeItem with type=POUR_OVER, description=\"Hand-poured coffee for document test\""
          putStrLn "Actual item: Nothing"
          HUnit.assertFailure "Coffee item should be present in document"
        Just actualCoffeeItem -> do
          putStrLn $ "Expected item: CoffeeItem with type=POUR_OVER, description=\"Hand-poured coffee for document test\""
          putStrLn $ "Actual item: " ++ show actualCoffeeItem

          let expectedCoffeeType = CoffeeType.POUR_OVER
              expectedDescription = "Hand-poured coffee for document test"
              actualCoffeeType = CoffeeItem.coffeeType actualCoffeeItem
              actualDescription = CoffeeItem.description actualCoffeeItem

          putStrLn $ "Expected coffee type: " ++ show expectedCoffeeType
          putStrLn $ "Actual coffee type: " ++ show actualCoffeeType
          putStrLn $ "Expected description: " ++ show expectedDescription
          putStrLn $ "Actual description: " ++ show actualDescription

          HUnit.assertEqual "Coffee type should match" expectedCoffeeType actualCoffeeType
          HUnit.assertEqual "Coffee description should match" expectedDescription actualDescription

      case TestHttpDocumentDeserializationOutput.customization output of
        Nothing -> do
          putStrLn "Expected customization: CoffeeCustomization with temperature=HOT"
          putStrLn "Actual customization: Nothing"
          HUnit.assertFailure "Coffee customization should be present in document"
        Just actualCustomization -> do
          putStrLn "Expected customization: CoffeeCustomization with temperature=HOT"
          putStrLn $ "Actual customization: " ++ show actualCustomization

          let expectedTemperature = TemperaturePreference.HOT
          case actualCustomization of
            CoffeeCustomization.Temperature t -> do
              HUnit.assertEqual "Temperature should match" expectedTemperature t
            _ ->
              HUnit.assertFailure "Unexpected customization type, expected Temperature"
