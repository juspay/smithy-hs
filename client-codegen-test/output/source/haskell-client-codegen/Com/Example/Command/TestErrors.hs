module Com.Example.Command.TestErrors (
    TestErrorsError (..),
    testErrors
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.Error400
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestErrorsInput
import qualified Com.Example.Model.TestErrorsOutput
import qualified Com.Example.Utility
import qualified Data.Aeson
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestErrorsError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | Error400 Com.Example.Model.Error400.Error400
    | BuilderError Data.Text.Text
    | DeSerializationError Com.Example.Utility.HttpMetadata Data.Text.Text
    | UnexpectedError (Data.Maybe.Maybe Com.Example.Utility.HttpMetadata) Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestErrorsError
instance Com.Example.Utility.OperationError TestErrorsError where
    mkDeSerializationError = DeSerializationError
    mkUnexpectedError = UnexpectedError

    getErrorParser status
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.InternalServerError.InternalServerError) = Just (fmap InternalServerError (Com.Example.Utility.responseParser @Com.Example.Model.InternalServerError.InternalServerError))
        | status == (Com.Example.Utility.expectedStatus @Com.Example.Model.Error400.Error400) = Just (fmap Error400 (Com.Example.Utility.responseParser @Com.Example.Model.Error400.Error400))
        | otherwise = Nothing


testErrors :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestErrorsInput.TestErrorsInputBuilder () -> IO (Either TestErrorsError Com.Example.Model.TestErrorsOutput.TestErrorsOutput)
testErrors client builder =
    let endpoint = Com.Example.ExampleServiceClient.endpointUri client
        manager = Com.Example.ExampleServiceClient.httpManager client
        token = Com.Example.ExampleServiceClient.token client
        setAuth = Com.Example.Utility.serHeader "Authorization" ("Bearer " <> token)
    in Com.Example.Utility.runOperation endpoint manager setAuth (Com.Example.Model.TestErrorsInput.build builder)

