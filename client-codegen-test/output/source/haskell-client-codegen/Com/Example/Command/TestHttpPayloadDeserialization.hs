module Com.Example.Command.TestHttpPayloadDeserialization (
    TestHttpPayloadDeserializationError (..),
    testHttpPayloadDeserialization
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpPayloadDeserializationInput
import qualified Com.Example.Model.TestHttpPayloadDeserializationOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpPayloadDeserializationError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Com.Example.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Com.Example.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestHttpPayloadDeserializationError
instance Com.Example.Utility.OperationError TestHttpPayloadDeserializationError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testHttpPayloadDeserialization :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpPayloadDeserializationInput.TestHttpPayloadDeserializationInputBuilder () -> IO (Either TestHttpPayloadDeserializationError Com.Example.Model.TestHttpPayloadDeserializationOutput.TestHttpPayloadDeserializationOutput)
testHttpPayloadDeserialization client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestHttpPayloadDeserializationInput.build builder)

