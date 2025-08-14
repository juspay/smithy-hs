module Com.Example.ExampleServiceClient (
    ExampleServiceClient,
    Com.Example.Utility.RawRequest,
    Com.Example.Utility.requestMethod,
    Com.Example.Utility.requestPath,
    Com.Example.Utility.requestQuery,
    Com.Example.Utility.requestHeaders,
    Com.Example.Utility.requestBody,
    Com.Example.Utility.RawResponse,
    Com.Example.Utility.responseStatus,
    Com.Example.Utility.responseHeaders,
    Com.Example.Utility.responseBody,
    Com.Example.Utility.HttpMetadata,
    Com.Example.Utility.rawRequest,
    Com.Example.Utility.rawResponse,
    endpointUri,
    httpManager,
    token,
    setEndpointuri,
    setHttpmanager,
    setToken,
    build,
    ExampleServiceClientBuilder
) where
import qualified Com.Example.Utility
import qualified Control.Monad.State.Strict
import qualified Data.Either
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified Network.HTTP.Client
import qualified Network.URI

data ExampleServiceClient = ExampleServiceClient {
    endpointUri :: Network.URI.URI,
    httpManager :: Network.HTTP.Client.Manager,
    token :: Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

data ExampleServiceClientBuilderState = ExampleServiceClientBuilderState {
    endpointUriBuilderState :: Data.Maybe.Maybe Network.URI.URI,
    httpManagerBuilderState :: Data.Maybe.Maybe Network.HTTP.Client.Manager,
    tokenBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ExampleServiceClientBuilderState
defaultBuilderState = ExampleServiceClientBuilderState {
    endpointUriBuilderState = Data.Maybe.Nothing,
    httpManagerBuilderState = Data.Maybe.Nothing,
    tokenBuilderState = Data.Maybe.Nothing
}

type ExampleServiceClientBuilder = Control.Monad.State.Strict.State ExampleServiceClientBuilderState

setEndpointuri :: Network.URI.URI -> ExampleServiceClientBuilder ()
setEndpointuri value =
   Control.Monad.State.Strict.modify (\s -> (s { endpointUriBuilderState = Data.Maybe.Just value }))

setHttpmanager :: Network.HTTP.Client.Manager -> ExampleServiceClientBuilder ()
setHttpmanager value =
   Control.Monad.State.Strict.modify (\s -> (s { httpManagerBuilderState = Data.Maybe.Just value }))

setToken :: Data.Text.Text -> ExampleServiceClientBuilder ()
setToken value =
   Control.Monad.State.Strict.modify (\s -> (s { tokenBuilderState = Data.Maybe.Just value }))

build :: ExampleServiceClientBuilder () -> Data.Either.Either Data.Text.Text ExampleServiceClient
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    endpointUri' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.endpointUri is a required property.") Data.Either.Right (endpointUriBuilderState st)
    httpManager' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.httpManager is a required property.") Data.Either.Right (httpManagerBuilderState st)
    token' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.token is a required property.") Data.Either.Right (tokenBuilderState st)
    Data.Either.Right (ExampleServiceClient { 
        endpointUri = endpointUri',
        httpManager = httpManager',
        token = token'
    })


