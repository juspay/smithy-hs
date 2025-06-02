{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Com.Example.ExampleServiceClient qualified as Client
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar as MVar
import Control.Concurrent.STM qualified as Stm
import Control.Monad qualified
import Data.Either (isLeft)
import Data.Function ((&))
import Data.Functor
import Data.Text qualified as T
import HttpHeaderTest (testHttpHeaders)
import HttpLabelTest (testHttpLabels)
import HttpQueryTest (testHttpQuery)
import Message (State (..), compareRequest, defaultResponse)
import Network.HTTP.Client.TLS qualified as TLS
import Network.URI qualified as URI
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Exit (exitFailure)
import Test.HUnit qualified as HUnit

-- httpQuery (ser)
-- int, bool, timestamp, string, list
--
-- httpLabel (ser)
-- int, bool, timestamp, string
--
-- httpHeader (ser, de)
-- int, bool, timestamp, string
--
-- httpPrefixHeader (ser, de)
-- Map of String
--
-- httpQueryParams (ser)
-- Map of String, Map of List String
--
-- httpPayload (ser, de)
-- int, string, struct, union, enum, timestamp, bool

port :: Int
port = 4321

createClient :: T.Text -> IO (Either T.Text Client.ExampleServiceClient)
createClient token = do
  manager <- TLS.newTlsManager
  let uriE = URI.parseURI $ "http://localhost:" ++ show port
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

tests :: State -> HUnit.Test
tests state =
  HUnit.TestList
    [ HUnit.TestLabel "Client Creation" $ testClientCreation state,
      HUnit.TestLabel "PostMenu Operation" $ testHttpLabels state,
      HUnit.TestLabel "HttpQuery Operation" $ testHttpQuery state,
      HUnit.TestLabel "HttpHeader Operation" $ testHttpHeaders state
    ]

app :: State -> Wai.Application
app state request responder = do
  putStrLn "Received request"
  putStrLn (show request)
  response <-
    Stm.atomically $
      Stm.takeTMVar (res state)
        <* Stm.writeTMVar (req state) request
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

main :: IO ()
main = do
  req <- Stm.newEmptyTMVarIO @Wai.Request
  res <- Stm.newEmptyTMVarIO @Wai.Response
  client <-
    (createClient "test-token") >>= \case
      Left err -> do
        putStrLn $ "Error creating client: " ++ T.unpack err
        exitFailure
      Right c -> pure c

  serverStarted <- MVar.newEmptyMVar :: IO (MVar Bool)

  let state = State {req, res, client}
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
