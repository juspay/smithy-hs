{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HttpLabelTest where

import Com.Example.Command.TestHttpLabels qualified as TestHttpLabels
import Com.Example.ExampleServiceClient qualified as Client
import Com.Example.Model.TestHttpLabelsInput qualified as TestHttpLabelsInput
import Control.Concurrent.STM qualified as Stm
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Message (State (..), defaultResponse)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Test.HUnit (assertEqual)
import Test.HUnit qualified as HUnit

testHttpLabels :: State -> HUnit.Test
testHttpLabels state = HUnit.TestCase $ do
  let identifierValue = 42
      enabledValue = True
      nameValue = "test-name"
      expectedMethod = "GET"
      expectedPath = ["path_params", T.pack $ show identifierValue, T.toLower (T.pack $ show enabledValue), nameValue]

  _ <- Stm.atomically $ Stm.writeTMVar (res state) defaultResponse

  result <- TestHttpLabels.testHttpLabels (client state) $ do
    TestHttpLabelsInput.setIdentifier identifierValue
    TestHttpLabelsInput.setEnabled enabledValue
    TestHttpLabelsInput.setName nameValue

  actualReq <- Stm.atomically $ Stm.takeTMVar (req state)

  HUnit.assertEqual "Path should be equal" expectedPath (Wai.pathInfo actualReq)

  HUnit.assertEqual "HTTP method should be GET" HTTP.methodGet (Wai.requestMethod actualReq)

  case result of
    Left (TestHttpLabels.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestHttpLabels.RequestError _) ->
      HUnit.assertFailure "Unexpected request error"
    Left (TestHttpLabels.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right output ->
      HUnit.assertBool "TestHttpLabels operation successful" True