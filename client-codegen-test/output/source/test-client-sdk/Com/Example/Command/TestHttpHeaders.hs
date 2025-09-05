module Com.Example.Command.TestHttpHeaders (
    TestHttpHeadersError (..),
    testHttpHeaders
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpHeadersInput
import qualified Com.Example.Model.TestHttpHeadersOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpHeadersError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Com.Example.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Com.Example.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestHttpHeadersError
instance Com.Example.Utility.OperationError TestHttpHeadersError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testHttpHeaders :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpHeadersInput.TestHttpHeadersInputBuilder () -> IO (Either TestHttpHeadersError Com.Example.Model.TestHttpHeadersOutput.TestHttpHeadersOutput)
testHttpHeaders client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestHttpHeadersInput.build builder)

