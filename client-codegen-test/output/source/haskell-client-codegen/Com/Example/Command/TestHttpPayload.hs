module Com.Example.Command.TestHttpPayload (
    TestHttpPayloadError (..),
    testHttpPayload
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpPayloadInput
import qualified Com.Example.Model.TestHttpPayloadOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpPayloadError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Com.Example.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Com.Example.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestHttpPayloadError
instance Com.Example.Utility.OperationError TestHttpPayloadError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testHttpPayload :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpPayloadInput.TestHttpPayloadInputBuilder () -> IO (Either TestHttpPayloadError Com.Example.Model.TestHttpPayloadOutput.TestHttpPayloadOutput)
testHttpPayload client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestHttpPayloadInput.build builder)

