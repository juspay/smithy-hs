module Com.Example.Command.TestQuery (
    TestQueryError (..),
    testQuery
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestQueryInput
import qualified Com.Example.Model.TestQueryOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestQueryError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | DeSerializationError Com.Example.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Com.Example.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestQueryError
instance Com.Example.Utility.OperationError TestQueryError where
    mkBuilderError = BuilderError
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | otherwise = Nothing


testQuery :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestQueryInput.TestQueryInputBuilder () -> IO (Either TestQueryError Com.Example.Model.TestQueryOutput.TestQueryOutput)
testQuery client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestQueryInput.build builder)

