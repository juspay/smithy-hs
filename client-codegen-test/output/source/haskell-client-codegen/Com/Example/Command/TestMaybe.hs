module Com.Example.Command.TestMaybe (
    TestMaybeError (..),
    testMaybe
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestMaybeInput
import qualified Com.Example.Model.TestMaybeOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestMaybeError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Com.Example.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Com.Example.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestMaybeError
instance Com.Example.Utility.OperationError TestMaybeError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testMaybe :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestMaybeInput.TestMaybeInputBuilder () -> IO (Either TestMaybeError Com.Example.Model.TestMaybeOutput.TestMaybeOutput)
testMaybe client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        auth = Com.Example.ExampleServiceClient.getAuth client
    in Com.Example.Utility.runOperation endpoint manager auth (Com.Example.Model.TestMaybeInput.build builder)

