{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpLabelTest where

import Com.Example.Command.TestHttpLabels qualified as TestHttpLabels
import Com.Example.Model.TestHttpLabelsInput qualified as TestHttpLabelsInput
import Control.Concurrent.STM qualified as Stm
import Data.Text qualified as T
import Message (State (..), defaultResponse, assertEqRequest, RequestInternal (RequestInternal))
import qualified Message as RI
import Network.HTTP.Types qualified as HTTP
import Test.HUnit qualified as HUnit
import Network.HTTP.Date (parseHTTPDate)
import Data.Maybe (fromJust)
import Data.Text.Encoding (decodeUtf8)

testHttpLabels :: State -> HUnit.Test
testHttpLabels state = HUnit.TestCase $ do
  let dateString = "Mon, 01 Jan 2024 12:00:00 GMT"
      identifierValue = 42
      enabledValue = True
      nameValue = "test-name"

      time = fromJust $ parseHTTPDate dateString
      expectedRequest = RequestInternal
        { RI.queryString = [],
          RI.pathInfo =
            [ "path_params"
            , T.pack $ show identifierValue 
            , T.toLower (T.pack $ show enabledValue) 
            , nameValue
            , decodeUtf8 dateString
            ],
          RI.requestMethod = HTTP.methodGet,
          RI.requestHeaders = [] }

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpLabels.testHttpLabels (client state) $ do
    TestHttpLabelsInput.setIdentifier identifierValue
    TestHttpLabelsInput.setEnabled enabledValue
    TestHttpLabelsInput.setName nameValue
    TestHttpLabelsInput.setTime time

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  assertEqRequest expectedRequest actualReq
  case result of
    Left (TestHttpLabels.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpLabels.RequestError _) ->
      HUnit.assertFailure "Unexpected request error"
    Left (TestHttpLabels.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right _ ->
      HUnit.assertBool "TestHttpLabels operation successful" True