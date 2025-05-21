module Com.Example.ExampleServiceClient (
    ExampleServiceClient,
    endpointUri,
    httpManager,
    token,
    setEndpointuri,
    setHttpmanager,
    setToken,
    build,
    ExampleServiceClientBuilder
) where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Network.HTTP.Client
import qualified Network.URI

data ExampleServiceClient = ExampleServiceClient {
    endpointUri :: Network.URI.URI,
    httpManager :: Network.HTTP.Client.Manager,
    token :: Data.Text.Text
}

data ExampleServiceClientBuilderState = ExampleServiceClientBuilderState {
    endpointUriBuilderState :: Data.Maybe.Maybe Network.URI.URI,
    httpManagerBuilderState :: Data.Maybe.Maybe Network.HTTP.Client.Manager,
    tokenBuilderState :: Data.Maybe.Maybe Data.Text.Text
}

defaultBuilderState :: ExampleServiceClientBuilderState
defaultBuilderState = ExampleServiceClientBuilderState {
    endpointUriBuilderState = Data.Maybe.Nothing,
    httpManagerBuilderState = Data.Maybe.Nothing,
    tokenBuilderState = Data.Maybe.Nothing
}

newtype ExampleServiceClientBuilder a = ExampleServiceClientBuilder {
    runExampleServiceClientBuilder :: ExampleServiceClientBuilderState -> (ExampleServiceClientBuilderState, a)
}

instance Data.Functor.Functor ExampleServiceClientBuilder where
    fmap f (ExampleServiceClientBuilder g) =
        ExampleServiceClientBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative ExampleServiceClientBuilder where
    pure a = ExampleServiceClientBuilder (\s -> (s, a))
    (ExampleServiceClientBuilder f) <*> (ExampleServiceClientBuilder g) = ExampleServiceClientBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad ExampleServiceClientBuilder where
    (ExampleServiceClientBuilder f) >>= g = ExampleServiceClientBuilder (\s ->
        let (s', a) = f s
            (ExampleServiceClientBuilder h) = g a
        in h s')

setEndpointuri :: Network.URI.URI -> ExampleServiceClientBuilder ()
setEndpointuri value =
   ExampleServiceClientBuilder (\s -> (s { endpointUriBuilderState = Data.Maybe.Just value }, ()))

setHttpmanager :: Network.HTTP.Client.Manager -> ExampleServiceClientBuilder ()
setHttpmanager value =
   ExampleServiceClientBuilder (\s -> (s { httpManagerBuilderState = Data.Maybe.Just value }, ()))

setToken :: Data.Text.Text -> ExampleServiceClientBuilder ()
setToken value =
   ExampleServiceClientBuilder (\s -> (s { tokenBuilderState = Data.Maybe.Just value }, ()))

build :: ExampleServiceClientBuilder () -> Data.Either.Either Data.Text.Text ExampleServiceClient
build builder = do
    let (st, _) = runExampleServiceClientBuilder builder defaultBuilderState
    endpointUri' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.endpointUri is a required property.") Data.Either.Right (endpointUriBuilderState st)
    httpManager' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.httpManager is a required property.") Data.Either.Right (httpManagerBuilderState st)
    token' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.ExampleServiceClient.ExampleServiceClient.token is a required property.") Data.Either.Right (tokenBuilderState st)
    Data.Either.Right (ExampleServiceClient { 
        endpointUri = endpointUri',
        httpManager = httpManager',
        token = token'
    })


