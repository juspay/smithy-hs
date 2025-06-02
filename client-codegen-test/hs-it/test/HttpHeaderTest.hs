{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpHeaderTest where

import Com.Example.Command.TestHttpHeaders qualified as TestHttpHeaders
import Com.Example.ExampleServiceClient qualified as Client
import Com.Example.Model.TestHttpHeadersInput qualified as TestHttpHeadersInput
import Control.Concurrent.STM qualified as Stm
import Data.ByteString.Char8 qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Char (toLower)
import Data.Function
import Data.List (find, sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Message (State (..), defaultResponse)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit

testHttpHeaders :: State -> HUnit.Test
testHttpHeaders state = HUnit.TestCase $ do
  let intHeaderValue = 42
      stringHeaderValue = "test-string-value"
      boolHeaderValue = True
      listHeaderValue = ["value1", "value2", "value3"]
      prefixHeadersValue = Map.fromList [("custom1", "prefix-value1"), ("custom2", "prefix-value2")]

      expectedMethod = HTTP.methodGet
      expectedPath = ["headers"]

      expectedHeaders =
        [ ("x-header-int", BS.pack $ show intHeaderValue),
          ("x-header-string", BS.pack $ T.unpack stringHeaderValue),
          ("x-header-bool", BS.pack $ map toLower $ show boolHeaderValue),
          ("x-header-list", BS.pack $ T.unpack $ T.intercalate "," listHeaderValue),
          ("x-prefix-custom1", BS.pack $ T.unpack $ prefixHeadersValue Map.! "custom1"),
          ("x-prefix-custom2", BS.pack $ T.unpack $ prefixHeadersValue Map.! "custom2"),
          ("authorization", "Bearer test-token")
        ]

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpHeaders.testHttpHeaders (client state) $ do
    TestHttpHeadersInput.setIntheader (Just intHeaderValue)
    TestHttpHeadersInput.setStringheader (Just stringHeaderValue)
    TestHttpHeadersInput.setBoolheader (Just boolHeaderValue)
    TestHttpHeadersInput.setListheader (Just listHeaderValue)
    TestHttpHeadersInput.setPrefixheaders (Just prefixHeadersValue)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  HUnit.assertEqual "Path should match" expectedPath (Wai.pathInfo actualReq)

  HUnit.assertEqual "HTTP method should be GET" expectedMethod (Wai.requestMethod actualReq)

  let actualHeaders =
        Wai.requestHeaders actualReq
          & map (\(n, v) -> (decodeUtf8 (CI.original n), v))

  putStrLn ""
  putStrLn $ "Actual headers: " ++ show actualHeaders
  putStrLn ""
  putStrLn $ "Expected headers:" ++ show expectedHeaders
  putStrLn ""

  HUnit.assertBool "Integer header should be present with correct value" $
    any (\(n, v) -> n == "x-header-int" && v == BS.pack (show intHeaderValue)) actualHeaders

  HUnit.assertBool "String header should be present with correct value" $
    any (\(n, v) -> n == "x-header-string" && v == BS.pack (T.unpack stringHeaderValue)) actualHeaders

  HUnit.assertBool "Boolean header should be present with correct value" $
    any (\(n, v) -> n == "x-header-bool" && v == BS.pack (map toLower $ show boolHeaderValue)) actualHeaders

  HUnit.assertBool "List header should be present with correct value" $
    any (\(n, v) -> n == "x-header-list" && v == BS.pack (T.unpack $ T.intercalate "," listHeaderValue)) actualHeaders

  HUnit.assertBool "Prefix headers should be present with correct values" $
    any (\(n, v) -> n == "x-prefix-custom1" && v == BS.pack (T.unpack $ prefixHeadersValue Map.! "custom1")) actualHeaders
      && any (\(n, v) -> n == "x-prefix-custom2" && v == BS.pack (T.unpack $ prefixHeadersValue Map.! "custom2")) actualHeaders

  case result of
    Left (TestHttpHeaders.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpHeaders.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpHeaders.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right output ->
      HUnit.assertBool "TestHttpHeaders operation successful" True