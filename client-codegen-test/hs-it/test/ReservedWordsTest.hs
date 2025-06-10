{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ReservedWordsTest where

import qualified Com.Example.Command.TestReservedWords as TestReservedWords
import qualified Com.Example.Model.TestReservedWordsInput as TestReservedWordsInput
import qualified Com.Example.Model.TestReservedWordsOutput as TestReservedWordsOutput
import Control.Concurrent.STM qualified as Stm
import Data.Text qualified as T
import Message (State (..))
import Network.HTTP.Types qualified as HTTP
import Test.HUnit qualified as HUnit
import qualified Data.Aeson as Aeson
import Network.Wai as Wai
import Data.Either.Extra (fromRight')

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

      payload = Aeson.object
        [ "type" Aeson..= type'
        , "data" Aeson..= data'
        , "as" Aeson..= as'
        , "case" Aeson..= case'
        , "class" Aeson..= class'
        , "default" Aeson..= default'
        , "deriving" Aeson..= deriving'
        , "do" Aeson..= do'
        , "else" Aeson..= else'
        , "hiding" Aeson..= hiding'
        , "if" Aeson..= if'
        , "import" Aeson..= import'
        , "in" Aeson..= in'
        , "infix" Aeson..= infix'
        , "infixl" Aeson..= infixl'
        , "infixr" Aeson..= infixr'
        , "instance" Aeson..= instance'
        , "let" Aeson..= let'
        , "module" Aeson..= module'
        , "newtype" Aeson..= newtype'
        , "of" Aeson..= of'
        , "qualified" Aeson..= qualified'
        , "then" Aeson..= then'
        , "where" Aeson..= where'
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
    Left (TestReservedWords.BuilderError err) ->
      HUnit.assertFailure $ "Builder error: " ++ T.unpack err
    Left (TestReservedWords.RequestError err) ->
      HUnit.assertFailure $ "Request error: " ++ T.unpack err
    Left (TestReservedWords.InternalServerError _) ->
      HUnit.assertFailure "Unexpected server error"
    Right output -> do
      HUnit.assertEqual "Output should match" expectedOutput output
