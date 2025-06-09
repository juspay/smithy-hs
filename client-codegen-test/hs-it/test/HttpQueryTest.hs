{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpQueryTest where

import Com.Example.Command.TestQuery qualified as TestQuery
import Com.Example.Model.TestQueryInput qualified as TestQueryInput
import Control.Concurrent.STM qualified as Stm
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Message (State (..), defaultResponse, assertEqRequest, RequestInternal (RequestInternal))
import qualified Message as RI
import Network.HTTP.Date (parseHTTPDate)
import Network.HTTP.Types qualified as HTTP
import Test.HUnit qualified as HUnit
import qualified Data.ByteString.Char8 as BS
import Data.Char

testHttpQuery :: State -> HUnit.Test
testHttpQuery state = HUnit.TestCase $ do
  let dateString = "Tue, 02 Jan 2024 15:30:00 GMT"
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
                ("custom1", Just "value1"),
                ("type", Just $ BS.pack coffeeTypeValue),
                ("page", Just $ BS.pack $ show pageValue),
                ("time", Just dateString),
                ("enabled", Just $ BS.pack $ map toLower (show enabledValue)),
                ("tags", Just "tag1"),
                ("tags", Just "tag2"),
                ("tags", Just "tag3")
              ],
            RI.pathInfo = ["query_params"],
            RI.requestMethod = HTTP.methodGet,
            RI.requestHeaders = [
              ("Authorization", "Bearer test-token")
            ]
          }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestQuery.testQuery (client state) $ do
    TestQueryInput.setPage (Just pageValue)
    TestQueryInput.setCoffeetype (Just $ T.pack coffeeTypeValue)
    TestQueryInput.setEnabled (Just enabledValue)
    TestQueryInput.setTags (Just tagsValue)
    TestQueryInput.setTime (Just time)
    TestQueryInput.setMapqueryparams (Just customQueryParams)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)
  assertEqRequest expectedRequest actualReq

  case result of
    Left (TestQuery.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestQuery.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestQuery.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right _ ->
      HUnit.assertBool "TestQuery operation successful" True
