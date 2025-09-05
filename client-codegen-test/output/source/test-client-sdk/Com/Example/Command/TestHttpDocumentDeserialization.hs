module Com.Example.Command.TestHttpDocumentDeserialization (
    TestHttpDocumentDeserializationError (..),
    testHttpDocumentDeserialization
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpDocumentDeserializationInput
import qualified Com.Example.Model.TestHttpDocumentDeserializationOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpDocumentDeserializationError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Com.Example.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Com.Example.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestHttpDocumentDeserializationError
instance Com.Example.Utility.OperationError TestHttpDocumentDeserializationError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testHttpDocumentDeserialization :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpDocumentDeserializationInput.TestHttpDocumentDeserializationInputBuilder () -> IO (Either TestHttpDocumentDeserializationError Com.Example.Model.TestHttpDocumentDeserializationOutput.TestHttpDocumentDeserializationOutput)
testHttpDocumentDeserialization client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestHttpDocumentDeserializationInput.build builder)

