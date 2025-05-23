module Main (main) where

import qualified Com.Example.Command.PostMenu as PostMenu
import qualified Com.Example.ExampleServiceClient as Client
import qualified Com.Example.Model.PostMenuInput as PostMenuInput
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.URI as URI
import qualified Test.HUnit as HUnit
import System.Exit (exitFailure)

createClient :: T.Text -> IO (Either T.Text Client.ExampleServiceClient)
createClient token = do
  manager <- TLS.newTlsManager
  let uriE = URI.parseURI "http://localhost:8080"
  case uriE of
    Nothing -> pure $ Left "Invalid URI"
    Just uri -> pure $ Client.build $ do
      Client.setEndpointuri uri
      Client.setHttpmanager manager
      Client.setToken token

testClientCreation :: HUnit.Test
testClientCreation = HUnit.TestCase $ do
  clientE <- createClient "test-token"
  case clientE of
    Left err -> HUnit.assertFailure $ "Failed to create client: " ++ T.unpack err
    Right _ -> return ()

testPostMenu :: HUnit.Test
testPostMenu = HUnit.TestCase $ do
  clientE <- createClient "test-token"
  case clientE of
    Left err ->
      HUnit.assertFailure $ "Failed to create client: " ++ T.unpack err

    Right client -> do
      result <- PostMenu.postMenu client $ do
         PostMenuInput.setSome "path"
         PostMenuInput.setPage (Just 1)
         PostMenuInput.setQueryparams (Just $ Map.fromList [("key", "value")])
      case result of
        Left (PostMenu.BuilderError err) ->
          HUnit.assertFailure $ "Builder error: " ++ T.unpack err

        Left (PostMenu.RequestError _) ->
          HUnit.assertBool "Expected request error" True

        Left (PostMenu.InternalServerError _) ->
          HUnit.assertFailure "Unexpected server error"

        Right _ ->
          HUnit.assertBool "PostMenu operation successful" True

tests :: HUnit.Test
tests = HUnit.TestList [
    HUnit.TestLabel "Client Creation" testClientCreation,
    HUnit.TestLabel "PostMenu Operation" testPostMenu
  ]

main :: IO ()
main = do
  putStrLn "Running PostMenu tests with HUnit..."
  counts <- HUnit.runTestTT tests
  putStrLn $ "Tests run: " ++ show (HUnit.cases counts)
  putStrLn $ "Failures: " ++ show (HUnit.failures counts)
  putStrLn $ "Errors: " ++ show (HUnit.errors counts)

  -- Return exit code based on test results
  if (HUnit.failures counts > 0 || HUnit.errors counts > 0)
    then exitFailure
    else return ()
