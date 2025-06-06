{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpHeaderTest where

import Com.Example.Command.TestHttpHeaders qualified as TestHttpHeaders
import Com.Example.Model.TestHttpHeadersInput qualified as TestHttpHeadersInput
import Control.Concurrent.STM qualified as Stm
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Message (RequestInternal (RequestInternal), State (..), defaultResponse, assertEqRequest)
import Message qualified as RI
import Network.HTTP.Date (parseHTTPDate)
import Network.HTTP.Types qualified as HTTP
import Test.HUnit qualified as HUnit

testHttpHeaders :: State -> HUnit.Test
testHttpHeaders state = HUnit.TestCase $ do
  let dateString = "Wed, 03 Jan 2024 09:15:00 GMT"
      intHeaderValue = 42
      stringHeaderValue = "test-string-value"
      boolHeaderValue = True
      listHeaderValue = ["value1", "value2", "value3"]
      prefixHeadersValue = Map.fromList [("custom1", "prefix-value1"), ("custom2", "prefix-value2")]
      time = fromJust $ parseHTTPDate dateString

      expectedRequest =
        RequestInternal
          { RI.queryString = [],
            RI.pathInfo = ["headers"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders =
              [ ("x-header-bool", BS.pack $ map toLower $ show boolHeaderValue),
                ("x-header-int", BS.pack $ show intHeaderValue),
                ("x-header-list", BS.pack $ T.unpack $ T.intercalate "," listHeaderValue),
                ("x-header-time", dateString),
                ("x-header-string", BS.pack $ T.unpack stringHeaderValue),
                ("x-prefix-custom1", BS.pack $ T.unpack $ prefixHeadersValue Map.! "custom1"),
                ("x-prefix-custom2", BS.pack $ T.unpack $ prefixHeadersValue Map.! "custom2")
                -- ("authorization", "Bearer test-token")
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

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  assertEqRequest expectedRequest actualReq

  case result of
    Left (TestHttpHeaders.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpHeaders.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestHttpHeaders.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right _ ->
      HUnit.assertBool "TestHttpHeaders operation successful" True