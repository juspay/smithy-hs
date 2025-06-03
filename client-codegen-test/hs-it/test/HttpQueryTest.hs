{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpQueryTest where

import Com.Example.Command.TestQuery qualified as TestQuery
import Com.Example.Model.TestQueryInput qualified as TestQueryInput
import Control.Concurrent.STM qualified as Stm
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Message (State (..), defaultResponse)
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.URI (parseQuery, queryToQueryText)
import Network.Wai qualified as Wai
import Test.HUnit qualified as HUnit

testHttpQuery :: State -> HUnit.Test
testHttpQuery state = HUnit.TestCase $ do
  let pageValue = 42
      coffeeTypeValue = "LATTE"
      enabledValue = True
      tagsValue = ["tag1", "tag2", "tag3"]
      customQueryParams = Map.fromList [("custom1", "value1"), ("enabled", "false")]

      expectedMethod = HTTP.methodGet
      expectedPath = ["query_params"]

      expectedQueryItems =
        [ ("query_literal", Just "some_query_literal_value"), -- Literal from URI template
          ("page", Just $ T.pack $ show pageValue),
          ("type", Just coffeeTypeValue),
          ("enabled", Just $ T.toLower $ T.pack $ show enabledValue),
          ("tags", Just "tag1"),
          ("tags", Just "tag2"),
          ("tags", Just "tag3"),
          ("custom1", Just "value1")
          -- enabledValue should override the one in customQueryParams
        ]

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestQuery.testQuery (client state) $ do
    TestQueryInput.setPage (Just pageValue)
    TestQueryInput.setCoffeetype (Just coffeeTypeValue)
    TestQueryInput.setEnabled (Just enabledValue)
    TestQueryInput.setTags (Just tagsValue)
    TestQueryInput.setMapqueryparams (Just customQueryParams)

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  HUnit.assertEqual "Path should match" expectedPath (Wai.pathInfo actualReq)

  HUnit.assertEqual "HTTP method should be GET" expectedMethod (Wai.requestMethod actualReq)

  let queryBytes = Wai.rawQueryString actualReq
      actualQueryItems = queryToQueryText $ parseQuery queryBytes

  putStrLn ""
  putStrLn $ "Actual query string: " ++ show queryBytes
  putStrLn ""
  putStrLn $ "Parsed query items: " ++ show actualQueryItems
  putStrLn ""
  putStrLn $ "Expected query items: " ++ show expectedQueryItems
  putStrLn ""

  HUnit.assertBool "All expected query parameters should be present" $
    all (\expectedItem -> expectedItem `elem` actualQueryItems) expectedQueryItems

  case result of
    Left (TestQuery.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestQuery.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestQuery.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right _ ->
      HUnit.assertBool "TestQuery operation successful" True