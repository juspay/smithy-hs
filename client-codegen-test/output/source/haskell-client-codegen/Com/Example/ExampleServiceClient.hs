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
    Com.Example.Utility.BearerAuth(..),
    Com.Example.Utility.BasicAuth(..),
    endpointUri,
    httpManager,
    bearerAuth,
    basicAuth,
    setEndpointuri,
    setHttpmanager,
    setBearerauth,
    setBasicauth,
    build,
    ExampleServiceClientBuilder,
    getAuth
) where
import qualified Com.Example.Utility
import qualified Control.Applicative
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
    bearerAuth :: Data.Maybe.Maybe Com.Example.Utility.BearerAuth,
    basicAuth :: Data.Maybe.Maybe Com.Example.Utility.BasicAuth
} deriving (
  GHC.Generics.Generic
  )

data ExampleServiceClientBuilderState = ExampleServiceClientBuilderState {
    endpointUriBuilderState :: Data.Maybe.Maybe Network.URI.URI,
    httpManagerBuilderState :: Data.Maybe.Maybe Network.HTTP.Client.Manager,
    bearerAuthBuilderState :: Data.Maybe.Maybe Com.Example.Utility.BearerAuth,
    basicAuthBuilderState :: Data.Maybe.Maybe Com.Example.Utility.BasicAuth
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: ExampleServiceClientBuilderState
defaultBuilderState = ExampleServiceClientBuilderState {
    endpointUriBuilderState = Data.Maybe.Nothing,
    httpManagerBuilderState = Data.Maybe.Nothing,
    bearerAuthBuilderState = Data.Maybe.Nothing,
    basicAuthBuilderState = Data.Maybe.Nothing
}

type ExampleServiceClientBuilder = Control.Monad.State.Strict.State ExampleServiceClientBuilderState

setEndpointuri :: Network.URI.URI -> ExampleServiceClientBuilder ()
setEndpointuri value =
   Control.Monad.State.Strict.modify (\s -> (s { endpointUriBuilderState = Data.Maybe.Just value }))

setHttpmanager :: Network.HTTP.Client.Manager -> ExampleServiceClientBuilder ()
setHttpmanager value =
   Control.Monad.State.Strict.modify (\s -> (s { httpManagerBuilderState = Data.Maybe.Just value }))

setBearerauth :: Data.Maybe.Maybe Com.Example.Utility.BearerAuth -> ExampleServiceClientBuilder ()
setBearerauth value =
   Control.Monad.State.Strict.modify (\s -> (s { bearerAuthBuilderState = value }))

setBasicauth :: Data.Maybe.Maybe Com.Example.Utility.BasicAuth -> ExampleServiceClientBuilder ()
setBasicauth value =
   Control.Monad.State.Strict.modify (\s -> (s { basicAuthBuilderState = value }))

build :: ExampleServiceClientBuilder () -> Data.Either.Either Data.Text.Text ExampleServiceClient
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    endpointUri' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.endpointUri is a required property.") Data.Either.Right (endpointUriBuilderState st)
    httpManager' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.httpManager is a required property.") Data.Either.Right (httpManagerBuilderState st)
    bearerAuth' <- Data.Either.Right (bearerAuthBuilderState st)
    basicAuth' <- Data.Either.Right (basicAuthBuilderState st)
    Data.Either.Right (ExampleServiceClient { 
        endpointUri = endpointUri',
        httpManager = httpManager',
        bearerAuth = bearerAuth',
        basicAuth = basicAuth'
    })

getAuth :: ExampleServiceClient -> Maybe Com.Example.Utility.DynAuth
getAuth client = (Nothing
    Control.Applicative.<|> (Com.Example.Utility.DynAuth <$> (bearerAuth client))
    Control.Applicative.<|> (Com.Example.Utility.DynAuth <$> (basicAuth client))
    )


