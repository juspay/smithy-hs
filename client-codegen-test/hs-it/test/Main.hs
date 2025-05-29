{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Com.Example.Command.PostMenu as PostMenu
import qualified Com.Example.ExampleServiceClient as Client
import qualified Com.Example.Model.PostMenuInput as PostMenuInput
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad
import Data.Function ((&))
import Data.Functor
import qualified Data.Text as T
import Message (State (..), compareRequest, defaultResponse)
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.URI as URI
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Exit (exitFailure)
import qualified Test.HUnit as HUnit

createClient :: T.Text -> IO (Either T.Text Client.ExampleServiceClient)
createClient token = do
  manager <- TLS.newTlsManager
  let uriE = URI.parseURI "http://localhost:4321"
  case uriE of
    Nothing -> pure $ Left "Invalid URI"
    Just uri -> pure $ Client.build $ do
      Client.setEndpointuri uri
      Client.setHttpmanager manager
      Client.setToken token

testClientCreation :: State -> HUnit.Test
testClientCreation _ = HUnit.TestCase $ do
  clientE <- createClient "test-token"
  case clientE of
    Left err -> HUnit.assertFailure $ "Failed to create client: " ++ T.unpack err
    Right _ -> return ()

testPostMenu :: State -> HUnit.Test
testPostMenu state = HUnit.TestCase $ do
  let expectedReq = Wai.defaultRequest

  clientE <- createClient "test-token"
  case clientE of
    Left err ->
      HUnit.assertFailure $ "Failed to create client: " ++ T.unpack err
    Right client -> do
      _ <-
        res state
          & flip Stm.writeTMVar defaultResponse
          & Stm.atomically

      result <- PostMenu.postMenu client $ do
        PostMenuInput.setSome "some-menu"
        PostMenuInput.setPage (Just 1)
        PostMenuInput.setExperimenttype "experiment"
        PostMenuInput.setStatus ["created"]

      _ <- (Stm.takeTMVar (req state) & Stm.atomically >>= compareRequest expectedReq) <&> HUnit.assertBool "Request Serde"

      case result of
        Left (PostMenu.BuilderError err) ->
          HUnit.assertFailure $ "Builder error: " ++ T.unpack err
        Left (PostMenu.RequestError _) ->
          HUnit.assertBool "Expected request error" True
        Left (PostMenu.InternalServerError _) ->
          HUnit.assertFailure "Unexpected server error"
        Right _ ->
          HUnit.assertBool "PostMenu operation successful" True


tests :: State -> HUnit.Test
tests state =
  HUnit.TestList
    [ HUnit.TestLabel "Client Creation" $ testClientCreation state,
      HUnit.TestLabel "PostMenu Operation" $ testPostMenu state
    ]

app :: State -> Wai.Application
app state request responder = do
  response <- Stm.atomically $
    Stm.takeTMVar (res state)
    <* Stm.writeTMVar (req state) request
  responder response

main :: IO ()
main = do
  req <- Stm.newEmptyTMVarIO @Wai.Request
  res <- Stm.newEmptyTMVarIO @Wai.Response

  serverStarted <- MVar.newEmptyMVar :: IO (MVar Bool)

  let port = 4321
  let state = State {req, res}
  putStrLn $ "Starting server on port " ++ show port

  _ <- forkIO $ do
    Warp.runSettings (
        Warp.defaultSettings
        & Warp.setPort port
        & Warp.setBeforeMainLoop (putStrLn "Server started. Signaling main thread." >> MVar.putMVar serverStarted True)
        ) (app state)

  putStrLn "Waiting for server to start..."
  takeMVar serverStarted
  putStrLn "Server started. Running PostMenu tests with HUnit..."

  counts <- HUnit.runTestTT $ tests state
  putStrLn $ "Tests run: " ++ show (HUnit.cases counts)
  putStrLn $ "Failures: " ++ show (HUnit.failures counts)
  putStrLn $ "Errors: " ++ show (HUnit.errors counts)

  -- Return exit code based on test results
  Control.Monad.when (HUnit.failures counts > 0 || HUnit.errors counts > 0) exitFailure
