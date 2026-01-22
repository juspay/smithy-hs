{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Com.Example.Command.TestCustomStatus                    as TestCustomStatus
import qualified Com.Example.Command.TestErrors                          as TestErrors
import qualified Com.Example.Command.TestHttpDocument                    as TestHttpDocument
import qualified Com.Example.Command.TestHttpDocumentDeserialization     as TestHttpDocumentDeserialization
import qualified Com.Example.Command.TestHttpHeaders                     as TestHttpHeaders
import qualified Com.Example.Command.TestHttpLabels                      as TestHttpLabels
import qualified Com.Example.Command.TestHttpPayload                     as TestHttpPayload
import qualified Com.Example.Command.TestHttpPayloadDeserialization      as TestHttpPayloadDeserialization
import qualified Com.Example.Command.TestMaybe                           as TestMaybe
import qualified Com.Example.Command.TestQuery                           as TestQuery
import qualified Com.Example.Command.TestReservedWords                   as TestReservedWords
import qualified Com.Example.ExampleServiceClient                        as Client
import qualified Com.Example.Model.CoffeeCustomization                   as CoffeeCustomization
import qualified Com.Example.Model.CoffeeItem                            as CoffeeItem
import qualified Com.Example.Model.CoffeeType                            as CoffeeType
import qualified Com.Example.Model.Error400                              as Error400
import qualified Com.Example.Model.InternalServerError                   as InternalServerError
import qualified Com.Example.Model.MilkType                              as MilkType
import qualified Com.Example.Model.TemperaturePreference                 as TemperaturePreference
import qualified Com.Example.Model.TestCustomStatusOutput                as TestCustomStatusOutput
import qualified Com.Example.Model.TestHttpDocumentDeserializationInput  as TestHttpDocumentDeserializationInput
import qualified Com.Example.Model.TestHttpDocumentDeserializationOutput as TestHttpDocumentDeserializationOutput
import qualified Com.Example.Model.TestHttpDocumentInput                 as TestHttpDocumentInput
import qualified Com.Example.Model.TestHttpHeadersInput                  as TestHttpHeadersInput
import qualified Com.Example.Model.TestHttpLabelsInput                   as TestHttpLabelsInput
import qualified Com.Example.Model.TestHttpPayloadDeserializationInput   as TestHttpPayloadDeserializationInput
import qualified Com.Example.Model.TestHttpPayloadDeserializationOutput  as TestHttpPayloadDeserializationOutput
import qualified Com.Example.Model.TestHttpPayloadInput                  as TestHttpPayloadInput
import qualified Com.Example.Model.TestMaybeInput                        as TestMaybeInput
import qualified Com.Example.Model.TestMaybeOutput                       as TestMaybeOutput
import qualified Com.Example.Model.TestQueryInput                        as TestQueryInput
import qualified Com.Example.Model.TestReservedWordsInput                as TestReservedWordsInput
import qualified Com.Example.Model.TestReservedWordsOutput               as TestReservedWordsOutput
import           Control.Concurrent                                      (forkIO)
import           Control.Concurrent.MVar                                 as MVar
import qualified Control.Concurrent.STM                                  as Stm
import qualified Control.Monad
import qualified Data.Aeson                                              as Aeson
import qualified Data.ByteString                                         as BS
import           Data.Char                                               (toLower)
import           Data.Either.Extra                                       (fromRight')
import           Data.Function                                           ((&))
import           Data.Functor
import qualified Data.Map                                                as Map
import           Data.Maybe                                              (fromJust)
import qualified Data.Text                                               as T
import qualified Data.Text.Encoding                                      as T
import           GHC.Exts                                                (fromString)
import           Message                                                 (RequestInternal (..),
                                                                          State (..),
                                                                          assertEqRequest,
                                                                          defaultResponse)
import qualified Message                                                 as RI
import qualified Network.HTTP.Client.TLS                                 as TLS
import           Network.HTTP.Date                                       (parseHTTPDate)
import qualified Network.HTTP.Types                                      as HTTP
import qualified Network.URI                                             as URI
import qualified Network.Wai                                             as Wai
import qualified Network.Wai.Handler.Warp                                as Warp
import           System.Exit                                             (exitFailure)
import qualified Test.HUnit                                              as HUnit

port :: Int
port = 4321

toTextValues :: (Functor f) => f BS.ByteString -> f T.Text
toTextValues = fmap T.decodeUtf8

encodeStrict :: (Aeson.ToJSON a) => a -> BS.ByteString
encodeStrict = BS.toStrict . Aeson.encode

createClient :: T.Text -> IO (Either T.Text Client.ExampleServiceClient)
createClient token = do
  manager <- TLS.newTlsManager
  let uriE = URI.parseURI $ "http://localhost:" ++ show port
  case uriE of
    Nothing -> pure $ Left "Invalid URI"
    Just uri -> pure $ Client.build $ do
      Client.setEndpointuri uri
      Client.setHttpmanager manager
      Client.setBearerauth (Just $ Client.BearerAuth token)

testClientCreation :: State -> HUnit.Test
testClientCreation _ = HUnit.TestCase $ do
  clientE <- createClient "test-token"
  case clientE of
    Left err -> HUnit.assertFailure $ "Failed to create client: " ++ T.unpack err
    Right _ -> return ()

tests :: State -> HUnit.Test
tests state =
  HUnit.TestList
    [ HUnit.TestLabel "Client Creation" $ testClientCreation state,
      HUnit.TestLabel "HttpLabels Operation" $ testHttpLabels state,
      HUnit.TestLabel "HttpQuery Operation" $ testHttpQuery state,
      HUnit.TestLabel "HttpHeaders Operation" $ testHttpHeaders state,
      HUnit.TestLabel "HttpPayload Operation" $ testHttpPayload state,
      HUnit.TestLabel "HttpDocument Operation" $ testHttpDocument state,
      HUnit.TestLabel "HttpPayloadDeserialization Operation" $ testHttpPayloadDeserialization state,
      HUnit.TestLabel "HttpDocumentDeserialization Operation" $ testHttpDocumentDeserialization state,
      HUnit.TestLabel "ReservedWords Operation" $ testReservedWords state,
      HUnit.TestLabel "CustomStatus Operation" $ testDeSerializationWithCustomStatus state,
      HUnit.TestLabel "TestMaybe Operation" $ testMaybe state,
      HUnit.TestLabel "Error Parsing" $ testErrorParsing state
    ]

{- TODO Add HTTP semantic assertions for things like `content-type`.
 - These kind of assertions don't belong in the test-defintions. -}
app :: State -> Wai.Application
app state request responder = do
  body <- Wai.consumeRequestBodyStrict request <&> BS.toStrict
  response <-
    Stm.atomically $
      Stm.takeTMVar (res state)
        <* Stm.writeTMVar (req state) request
        <* Stm.writeTMVar (rBody state) body
  responder response

serverSettings :: MVar Bool -> Warp.Settings
serverSettings serverStarted =
  Warp.defaultSettings
    & Warp.setPort port
    & Warp.setBeforeMainLoop afterServerStart
  where
    afterServerStart =
      putStrLn "Server started. Signaling main thread."
        >> MVar.putMVar serverStarted True

-- TODO Add test for handling custom-status codes.
main :: IO ()
main = do
  req <- Stm.newEmptyTMVarIO @Wai.Request
  res <- Stm.newEmptyTMVarIO @Wai.Response
  rBody <- Stm.newEmptyTMVarIO @BS.ByteString

  client <-
    createClient "test-token" >>= \case
      Left err -> do
        putStrLn $ "Error creating client: " ++ T.unpack err
        exitFailure
      Right c -> pure c

  serverStarted <- MVar.newEmptyMVar :: IO (MVar Bool)

  let state = State {req, res, rBody, client}
  putStrLn $ "Starting server on port " ++ show port

  _ <- forkIO $ Warp.runSettings (serverSettings serverStarted) (app state)
  _ <- takeMVar serverStarted
  putStrLn "Server started. Running PostMenu tests with HUnit..."

  counts <- HUnit.runTestTT $ tests state
  putStrLn $ "Tests run: " ++ show (HUnit.cases counts)
  putStrLn $ "Failures: " ++ show (HUnit.failures counts)
  putStrLn $ "Errors: " ++ show (HUnit.errors counts)

  -- Return exit code based on test results
  Control.Monad.when (HUnit.failures counts > 0 || HUnit.errors counts > 0) exitFailure

testHttpPayload :: State -> HUnit.Test
testHttpPayload state = HUnit.TestCase $ do
  let dateString = "Thu, 04 Jan 2024 14:45:00 GMT"
      identifierValue = 123
      stringHeaderValue = "test-header-value"
      prefixHeadersValue = Map.fromList [("custom", "prefix-value")]
      time = fromJust $ parseHTTPDate dateString
      utc = "2025-08-28T07:46:50.041601239Z"
      posix = "1.756368077735519317e9"

      coffeeItem = fromRight' $ CoffeeItem.build $ do
        CoffeeItem.setCoffeetype CoffeeType.LATTE
        CoffeeItem.setDescription "A smooth latte with silky foam"
        CoffeeItem.setCreatedat time
        CoffeeItem.setUtc (Just $ fromJust $ Aeson.decodeStrict $ "\"" <> utc <> "\"")
        CoffeeItem.setPosix (Just $ fromJust $ Aeson.decodeStrict posix)

      expectedPayload = Aeson.toJSON coffeeItem
      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["payload", T.pack $ show identifierValue],
            RI.requestMethod = HTTP.methodPost,
            RI.requestHeaders =
              [ ("x-header-string", T.encodeUtf8 stringHeaderValue),
                ("x-prefix-custom", prefixHeadersValue Map.! "custom"),
                ("content-type", "application/json"),
                ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) (Wai.responseLBS HTTP.ok200 [] "{ \"message\": \"Success\" }")

  result <- TestHttpPayload.testHttpPayload (client state) $ do
    TestHttpPayloadInput.setPayload coffeeItem
    TestHttpPayloadInput.setIdentifier identifierValue
    TestHttpPayloadInput.setStringheader (Just stringHeaderValue)
    TestHttpPayloadInput.setPrefixheaders (Just $ toTextValues prefixHeadersValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)

  assertEqRequest expectedRequest actualReq

  case Aeson.decodeStrict actualPayload of
    Just actualJson -> do
      HUnit.assertEqual "Payload JSON should match" expectedPayload actualJson
    _ -> HUnit.assertFailure "Failed to parse JSON payload"

  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right _ ->
      HUnit.assertBool "TestHttpPayload operation successful" True

testHttpQuery :: State -> HUnit.Test
testHttpQuery state = HUnit.TestCase $ do
  let dateString = "Tue, 02 Jan 2024 15:30:00 GMT"
      utc = "2025-08-28T07:46:50.041601239Z"
      posix = "1.756368077735519317e9"
      time = fromJust $ parseHTTPDate dateString
      pageValue = 42
      coffeeTypeValue = "LATTE"
      enabledValue = True
      tagsValue = ["tag1", "tag2", "tag3"]
      customQueryParams = Map.fromList [("custom1", "value1"), ("enabled", "false")]

      expectedRequest =
        RequestInternal
          { RI.queryString =
              [ ("query_literal", Just "some_query_literal_value"),
                ("utc", Just utc),
                ("custom1", Just "value1"),
                ("posix-ts", Just posix),
                ("type", Just coffeeTypeValue),
                ("page", Just $ fromString $ show pageValue),
                ("time", Just dateString),
                ("enabled", Just $ BS.toStrict $ Aeson.encode enabledValue),
                ("tags", Just "tag1"),
                ("tags", Just "tag2"),
                ("tags", Just "tag3")
              ],
            RI.pathInfo = ["query_params"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders =
              [ ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestQuery.testQuery (client state) $ do
    TestQueryInput.setPage (Just pageValue)
    TestQueryInput.setCoffeetype (Just $ T.decodeUtf8 coffeeTypeValue)
    TestQueryInput.setEnabled (Just enabledValue)
    TestQueryInput.setTags (Just tagsValue)
    TestQueryInput.setTime (Just time)
    TestQueryInput.setMapqueryparams (Just customQueryParams)
    -- Serialized UTC time has to be a stringified JSON value, quoted.
    TestQueryInput.setUtc (Aeson.decodeStrict $ "\"" <> utc <> "\"")
    TestQueryInput.setPosixts (Aeson.decodeStrict posix)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  assertEqRequest expectedRequest actualReq

  case result of
    Left _ -> pure ()
    Right _ ->
      HUnit.assertBool "TestQuery operation successful" True

testHttpPayloadDeserialization :: State -> HUnit.Test
testHttpPayloadDeserialization state = HUnit.TestCase $ do
  let coffeeTypeValue = "ESPRESSO"
      dateString = "Fri, 05 Jan 2024 16:20:00 GMT"
      utc = "2025-08-28T07:46:50.041601239Z"
      posix = "1.756368077735519317e9"

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
        CoffeeItem.setUtc (Just $ fromJust $ Aeson.decodeStrict $ "\"" <> utc <> "\"")
        CoffeeItem.setPosix (Just $ fromJust $ Aeson.decodeStrict posix)

      expectedOutput = fromRight' $ TestHttpPayloadDeserializationOutput.build $ do
        TestHttpPayloadDeserializationOutput.setOutputheader (Just expectedOutputHeader)
        TestHttpPayloadDeserializationOutput.setOutputheaderint (Just expectedOutputHeaderInt)
        TestHttpPayloadDeserializationOutput.setOutputheaderbool (Just expectedOutputHeaderBool)
        TestHttpPayloadDeserializationOutput.setOutputheaderlist (Just expectedOutputHeaderList)
        TestHttpPayloadDeserializationOutput.setTime (Just expectedTimeValue)
        TestHttpPayloadDeserializationOutput.setOutputprefixheaders (Just expectedOutputPrefixHeaders)
        TestHttpPayloadDeserializationOutput.setItem (Just expectedCoffeeItem)
        TestHttpPayloadDeserializationOutput.setUtcheader (Just $ fromJust $ Aeson.decodeStrict $ "\"" <> utc <> "\"")
        TestHttpPayloadDeserializationOutput.setPosixheader (Just $ fromJust $ Aeson.decodeStrict posix)

      expectedReq =
        RequestInternal
          { RI.requestMethod = HTTP.methodGet,
            RI.pathInfo = ["payload_response"],
            RI.queryString = [("type", Just coffeeTypeValue)],
            RI.requestHeaders =
              [ ("Authorization", "Bearer test-token")
              ]
          }

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", T.encodeUtf8 expectedOutputHeader),
            ("x-output-header-int", fromString $ show expectedOutputHeaderInt),
            ("x-output-header-bool", fromString $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", T.encodeUtf8 $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-header-time", dateString),
            ("x-output-header-utc", utc),
            ("x-output-header-posix", posix),
            ("x-output-prefix-custom", T.encodeUtf8 $ expectedOutputPrefixHeaders Map.! "custom"),
            ("x-output-prefix-another", T.encodeUtf8 $ expectedOutputPrefixHeaders Map.! "another"),
            ("Content-Type", "application/json")
          ]
          (Aeson.encode expectedCoffeeItem)

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestHttpPayloadDeserialization.testHttpPayloadDeserialization (client state) $ do
    TestHttpPayloadDeserializationInput.setCoffeetype (Just $ T.decodeUtf8 coffeeTypeValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedReq actualReq
  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output

testHttpLabels :: State -> HUnit.Test
testHttpLabels state = HUnit.TestCase $ do
  let dateString = "Mon, 01 Jan 2024 12:00:00 GMT"
      utc = "2025-08-28T07:46:50.041601239Z"
      posix = "1.756368077735519317e9"
      identifierValue = 42
      enabledValue = True
      nameValue = "test-name"

      time = fromJust $ parseHTTPDate dateString
      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo =
              [ "path_params",
                T.pack $ show identifierValue,
                T.toLower (T.pack $ show enabledValue),
                nameValue,
                T.decodeUtf8 dateString,
                T.decodeUtf8 utc,
                T.decodeUtf8 posix
              ],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders =
              [ ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpLabels.testHttpLabels (client state) $ do
    TestHttpLabelsInput.setIdentifier identifierValue
    TestHttpLabelsInput.setEnabled enabledValue
    TestHttpLabelsInput.setName nameValue
    TestHttpLabelsInput.setTime time
    TestHttpLabelsInput.setUtc (fromJust $ Aeson.decodeStrict $ "\"" <> utc <> "\"")
    TestHttpLabelsInput.setPosix (fromJust $ Aeson.decodeStrict posix)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedRequest actualReq
  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right _ ->
      HUnit.assertBool "TestHttpLabels operation successful" True

testHttpHeaders :: State -> HUnit.Test
testHttpHeaders state = HUnit.TestCase $ do
  let dateString = "Wed, 03 Jan 2024 09:15:00 GMT"
      intHeaderValue = 42
      stringHeaderValue = "test-string-value"
      boolHeaderValue = True
      listHeaderValue = ["value1", "value2", "value3"]
      prefixHeadersValue = Map.fromList [("custom1", "prefix-value1"), ("custom2", "prefix-value2")]
      time = fromJust $ parseHTTPDate dateString
      utc = "2025-08-28T07:46:50.041601239Z"
      posix = "1.756368077735519317e9"

      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["headers"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders =
              [ ("x-header-bool", encodeStrict boolHeaderValue),
                ("x-header-int", encodeStrict intHeaderValue),
                ("x-header-list", T.encodeUtf8 $ T.intercalate "," listHeaderValue),
                ("x-header-time", dateString),
                ("x-header-utc", utc),
                ("x-header-posix", posix),
                ("x-header-string", T.encodeUtf8 stringHeaderValue),
                ("x-prefix-custom1", T.encodeUtf8 $ prefixHeadersValue Map.! "custom1"),
                ("x-prefix-custom2", T.encodeUtf8 $ prefixHeadersValue Map.! "custom2"),
                ("Authorization", "Bearer test-token")
              ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpHeaders.testHttpHeaders (client state) $ do
    TestHttpHeadersInput.setIntheader (Just intHeaderValue)
    TestHttpHeadersInput.setStringheader (Just stringHeaderValue)
    TestHttpHeadersInput.setBoolheader (Just boolHeaderValue)
    TestHttpHeadersInput.setListheader (Just listHeaderValue)
    TestHttpHeadersInput.setTime (Just time)
    TestHttpHeadersInput.setPrefixheaders (Just prefixHeadersValue)
    TestHttpHeadersInput.setUtc (Aeson.decodeStrict $ "\"" <> utc <> "\"")
    TestHttpHeadersInput.setPosix (Aeson.decodeStrict posix)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  assertEqRequest expectedRequest actualReq

  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right _ ->
      HUnit.assertBool "TestHttpHeaders operation successful" True

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

      expectedPayloadJson =
        Aeson.object
          [ "payload" Aeson..= coffeeItem,
            "customization" Aeson..= customization,
            "time" Aeson..= T.decodeUtf8 dateString
          ]

      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["document", T.pack $ show identifierValue],
            RI.requestMethod = HTTP.methodPost,
            RI.requestHeaders =
              [ ("x-header-string", T.encodeUtf8 stringHeaderValue),
                ("x-prefix-doc", T.encodeUtf8 $ prefixHeadersValue Map.! "doc"),
                ("content-type", "application/json"),
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
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right _ ->
      HUnit.assertBool "TestHttpDocument operation successful" True

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

      expectedRequest =
        RequestInternal
          { RI.queryString = [("type", Just coffeeTypeValue)],
            RI.pathInfo = ["document_response"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders = [("Authorization", "Bearer test-token")]
          }

      mockResponse =
        Wai.responseLBS
          HTTP.status200
          [ ("x-output-header", fromString expectedOutputHeader),
            ("x-output-header-int", fromString $ show expectedOutputHeaderInt),
            ("x-output-header-bool", fromString $ map toLower $ show expectedOutputHeaderBool),
            ("x-output-header-list", T.encodeUtf8 $ T.intercalate "," expectedOutputHeaderList),
            ("x-output-prefix-metadata", T.encodeUtf8 $ expectedOutputPrefixHeaders Map.! "metadata"),
            ("x-output-prefix-version", T.encodeUtf8 $ expectedOutputPrefixHeaders Map.! "version"),
            ("x-output-header-time", dateString),
            ("Content-Type", "application/json")
          ]
          ( Aeson.object
              [ "item" Aeson..= expectedCoffeeItem,
                "customization" Aeson..= expectedCustomization,
                "time" Aeson..= T.decodeUtf8 dateString
              ]
              & Aeson.encode
          )

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
    TestHttpDocumentDeserializationInput.setCoffeetype (Just $ T.decodeUtf8 coffeeTypeValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedRequest actualReq

  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output

testReservedWords :: State -> HUnit.Test
testReservedWords state = HUnit.TestCase $ do
  let type' = "type"
      data' = "data"
      as' = "as"
      case' = "case"
      class' = "class"
      default' = "default"
      deriving' = "deriving"
      do' = "do"
      else' = "else"
      hiding' = "hiding"
      if' = "if"
      import' = "import"
      in' = "in"
      infix' = "infix"
      infixl' = "infixl"
      infixr' = "infixr"
      instance' = "instance"
      let' = "let"
      module' = "module"
      newtype' = "newtype"
      of' = "of"
      qualified' = "qualified"
      then' = "then"
      where' = "where"

      payload =
        Aeson.object
          [ "type" Aeson..= type',
            "data" Aeson..= data',
            "as" Aeson..= as',
            "case" Aeson..= case',
            "class" Aeson..= class',
            "default" Aeson..= default',
            "deriving" Aeson..= deriving',
            "do" Aeson..= do',
            "else" Aeson..= else',
            "hiding" Aeson..= hiding',
            "if" Aeson..= if',
            "import" Aeson..= import',
            "in" Aeson..= in',
            "infix" Aeson..= infix',
            "infixl" Aeson..= infixl',
            "infixr" Aeson..= infixr',
            "instance" Aeson..= instance',
            "let" Aeson..= let',
            "module" Aeson..= module',
            "newtype" Aeson..= newtype',
            "of" Aeson..= of',
            "qualified" Aeson..= qualified',
            "then" Aeson..= then',
            "where" Aeson..= where'
          ]

      expectedOutput = fromRight' $ TestReservedWordsOutput.build $ do
        TestReservedWordsOutput.setType' type'
        TestReservedWordsOutput.setData' data'
        TestReservedWordsOutput.setAs' as'
        TestReservedWordsOutput.setCase' case'
        TestReservedWordsOutput.setClass' class'
        TestReservedWordsOutput.setDefault' default'
        TestReservedWordsOutput.setDeriving' deriving'
        TestReservedWordsOutput.setDo' do'
        TestReservedWordsOutput.setElse' else'
        TestReservedWordsOutput.setHiding' hiding'
        TestReservedWordsOutput.setIf' if'
        TestReservedWordsOutput.setImport' import'
        TestReservedWordsOutput.setIn' in'
        TestReservedWordsOutput.setInfix' infix'
        TestReservedWordsOutput.setInfixl' infixl'
        TestReservedWordsOutput.setInfixr' infixr'
        TestReservedWordsOutput.setInstance' instance'
        TestReservedWordsOutput.setLet' let'
        TestReservedWordsOutput.setModule' module'
        TestReservedWordsOutput.setNewtype' newtype'
        TestReservedWordsOutput.setOf' of'
        TestReservedWordsOutput.setQualified' qualified'
        TestReservedWordsOutput.setThen' then'
        TestReservedWordsOutput.setWhere' where'

      mockResponse = Wai.responseLBS HTTP.status200 [] (Aeson.encode payload)

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestReservedWords.testReservedWords (client state) $ do
    TestReservedWordsInput.setType' type'
    TestReservedWordsInput.setData' data'
    TestReservedWordsInput.setAs' as'
    TestReservedWordsInput.setCase' case'
    TestReservedWordsInput.setClass' class'
    TestReservedWordsInput.setDefault' default'
    TestReservedWordsInput.setDeriving' deriving'
    TestReservedWordsInput.setDo' do'
    TestReservedWordsInput.setElse' else'
    TestReservedWordsInput.setHiding' hiding'
    TestReservedWordsInput.setIf' if'
    TestReservedWordsInput.setImport' import'
    TestReservedWordsInput.setIn' in'
    TestReservedWordsInput.setInfix' infix'
    TestReservedWordsInput.setInfixl' infixl'
    TestReservedWordsInput.setInfixr' infixr'
    TestReservedWordsInput.setInstance' instance'
    TestReservedWordsInput.setLet' let'
    TestReservedWordsInput.setModule' module'
    TestReservedWordsInput.setNewtype' newtype'
    TestReservedWordsInput.setOf' of'
    TestReservedWordsInput.setQualified' qualified'
    TestReservedWordsInput.setThen' then'
    TestReservedWordsInput.setWhere' where'

  actualPayload <- Stm.atomically $ Stm.takeTMVar (rBody state)
  case Aeson.decodeStrict actualPayload of
    Just actualJson -> do
      HUnit.assertEqual "Payload should match" payload actualJson
    _ -> HUnit.assertFailure "Failed to parse JSON document"

  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output

testDeSerializationWithCustomStatus :: State -> HUnit.Test
testDeSerializationWithCustomStatus state = HUnit.TestCase $ do
  let mockResponse = Wai.responseLBS HTTP.status201 [] "{\"message\": \"Custom Status Response\"}"
  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse
  result <- TestCustomStatus.testCustomStatus (client state) (pure ())
  case result of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right output -> do
      HUnit.assertEqual "Output should match" "Custom Status Response" (TestCustomStatusOutput.message output)
  let mockResponse2 = Wai.responseLBS HTTP.status200 [] "{\"message\": \"Accepted Response\"}"
  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse2
  result2 <- TestCustomStatus.testCustomStatus (client state) (pure ())
  case result2 of
    Left e ->
      HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right output -> do
      HUnit.assertEqual "Output should match" "Accepted Response" (TestCustomStatusOutput.message output)

testMaybe :: State -> HUnit.Test
testMaybe state = HUnit.TestCase $ do
  -- Test with both required and optional fields set
  let mockResponse1 = Wai.responseLBS HTTP.status200 [] "{\"testField\": \"required value\", \"testMaybeField\": \"optional value\"}"
  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse1

  result1 <- TestMaybe.testMaybe (client state) $ do
    TestMaybeInput.setTestfield "required value"
    TestMaybeInput.setTestmaybefield $ Just "optional value"

  actualReq1 <- Stm.atomically $ Stm.takeTMVar (req state)
  actualPayload1 <- Stm.atomically $ Stm.takeTMVar (rBody state)

  -- Verify the request
  let expectedRequest1 =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["test-maybe"],
            RI.requestMethod = HTTP.methodPost,
            RI.requestHeaders =
              [ ("content-type", "application/json"),
                ("Authorization", "Bearer test-token")
              ]
          }
  assertEqRequest expectedRequest1 actualReq1

  -- Verify the payload includes both fields
  case Aeson.decodeStrict actualPayload1 of
    Nothing -> HUnit.assertFailure "Failed to decode payload"
    Just (payload :: Aeson.Value) -> do
      let expectedPayload = Aeson.object [("testField", Aeson.String "required value"), ("testMaybeField", Aeson.String "optional value")]
      HUnit.assertEqual "Payload should match" expectedPayload payload

  -- Verify the result
  case result1 of
    Left e -> HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right output -> do
      HUnit.assertEqual "testField should match" "required value" (TestMaybeOutput.testField output)
      HUnit.assertEqual "testMaybeField should match" (Just "optional value") (TestMaybeOutput.testMaybeField output)

  -- Test with Nothing for testMaybeField (but testField is required)
  let mockResponse2 = Wai.responseLBS HTTP.status200 [] "{\"testField\": \"required value\"}"
  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse2

  result2 <- TestMaybe.testMaybe (client state) $ do
    TestMaybeInput.setTestfield "required value"
    TestMaybeInput.setTestmaybefield Nothing

  actualReq2 <- Stm.atomically $ Stm.takeTMVar (req state)
  actualPayload2 <- Stm.atomically $ Stm.takeTMVar (rBody state)

  -- Verify the request
  assertEqRequest expectedRequest1 actualReq2

  -- Verify the payload includes only the required field
  case Aeson.decodeStrict actualPayload2 of
    Nothing -> HUnit.assertFailure "Failed to decode payload"
    Just (payload :: Aeson.Value) -> do
      let expectedPayload = Aeson.object [("testField", Aeson.String "required value")]
      HUnit.assertEqual "Payload should include only required field" expectedPayload payload

  -- Verify the result when response has no testMaybeField
  case result2 of
    Left e -> HUnit.assertFailure $ "Error: " <> show (Aeson.encode e)
    Right output -> do
      HUnit.assertEqual "testField should match" "required value" (TestMaybeOutput.testField output)
      HUnit.assertEqual "testMaybeField should be Nothing" Nothing (TestMaybeOutput.testMaybeField output)

testErrorParsing :: State -> HUnit.Test
testErrorParsing state = HUnit.TestCase $ do
  let mockResponse = Wai.responseLBS HTTP.status400 [("Content-Type", "application/json")] "{\"message\": \"Bad Request\"}"

  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse

  result <- TestErrors.testErrors (client state) (pure ())
  let expectedError = fromRight' $ Error400.build $ Error400.setMessage "Bad Request"

  case result of
    Left (TestErrors.Error400 err) ->
      HUnit.assertEqual "Error message should match" expectedError err
    _ ->
      HUnit.assertFailure $ "Expected Error400, got: " <> show (Aeson.encode result)

  let mockResponse2 = Wai.responseLBS HTTP.status500 [("Content-Type", "application/json")] "{\"message\": \"Internal Server Error\"}"
  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse2
  result2 <- TestErrors.testErrors (client state) (pure ())

  let expectedInternalError = fromRight' $ InternalServerError.build $ InternalServerError.setMessage $ Just "Internal Server Error"

  case result2 of
    Left (TestErrors.InternalServerError e) ->
      HUnit.assertEqual "Error body should match" expectedInternalError e
    _ ->
      HUnit.assertFailure $ "Expected InternalServerError got: " <> show (Aeson.encode result2)

  let mockResponse3 = Wai.responseLBS HTTP.status502 [("Content-Type", "application/json")] "{\"message\": \"Bad Gateway\"}"
  _ <- Stm.atomically $ Stm.writeTMVar (res state) mockResponse3

  result3 <- TestErrors.testErrors (client state) (pure ())

  case result3 of
    Left (TestErrors.UnexpectedError _ _) ->
      HUnit.assertBool "UnexpectedStatus as expected" True
    _ ->
      HUnit.assertFailure $ "Expected UnexpectedStatus, got: " <> show (Aeson.encode result3)
