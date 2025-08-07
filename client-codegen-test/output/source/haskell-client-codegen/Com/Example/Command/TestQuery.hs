module Com.Example.Command.TestQuery (
    TestQueryError(..),
    testQuery
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestQueryInput
import qualified Com.Example.Model.TestQueryOutput
import qualified Com.Example.Utility
import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.Bifunctor
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.CaseInsensitive
import qualified Data.Either
import qualified Data.Function
import qualified Data.Functor
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data TestQueryError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestQueryError
instance Data.Aeson.FromJSON TestQueryError

serTestQueryQUERY :: Com.Example.Model.TestQueryInput.TestQueryInput -> Data.ByteString.ByteString
serTestQueryQUERY input =
    let
        staticParams = [
            toQueryItem ("query_literal", "some_query_literal_value")
            ]
        
        mapParams = Com.Example.Model.TestQueryInput.mapQueryParams input
                    Data.Function.& Data.Maybe.maybe [] (Data.Map.toList)
                    Data.Function.& (Data.List.filter (\(k, _) -> not $ Data.List.any (== k) reservedParams))
                    Data.Function.& toQuery
        
        coffeeTypeQuery = Com.Example.Model.TestQueryInput.coffeeType input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Com.Example.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("type", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        pageQuery = Com.Example.Model.TestQueryInput.page input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Com.Example.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("page", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        timeQuery = Com.Example.Model.TestQueryInput.time input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Com.Example.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("time", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        enabledQuery = Com.Example.Model.TestQueryInput.enabled input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Com.Example.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("enabled", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        tagsQuery = Com.Example.Model.TestQueryInput.tags input
                    Data.Functor.<&> Data.List.map (Com.Example.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("tags", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ mapParams ++ coffeeTypeQuery ++ pageQuery ++ timeQuery ++ enabledQuery ++ tagsQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
        reservedParams = [
            "query_literal",
            "type",
            "page",
            "time",
            "enabled",
            "tags"
            ]
        
    

serTestQueryLABEL :: Com.Example.Model.TestQueryInput.TestQueryInput -> Data.ByteString.ByteString
serTestQueryLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "query_params"
        ]
    

testQuery :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestQueryInput.TestQueryInputBuilder () -> IO (Data.Either.Either TestQueryError Com.Example.Model.TestQueryOutput.TestQueryOutput)
testQuery client inputB = do
    let inputE = Com.Example.Model.TestQueryInput.build inputB
        baseUri = Com.Example.ExampleServiceClient.endpointUri client
        httpManager = Com.Example.ExampleServiceClient.httpManager client
        requestE = Network.HTTP.Client.requestFromURI @(Data.Either.Either Control.Exception.SomeException) baseUri
    
    case (inputE, requestE) of
        (Data.Either.Left err, _) -> return $ Data.Either.Left (BuilderError err)
        (_, Data.Either.Left err) -> return $ Data.Either.Left (RequestError $ Data.Text.pack $ show err)
        (Data.Either.Right input, Data.Either.Right req) -> do
            response <- Network.HTTP.Client.httpLbs (toRequest input req) httpManager
            return $ Data.Bifunctor.first (RequestError) $ deserializeResponse response
        
    
    where
        method = Network.HTTP.Types.Method.methodGet
        token = Data.Text.Encoding.encodeUtf8 $ Com.Example.ExampleServiceClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serTestQueryLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serTestQueryQUERY input
                , Network.HTTP.Client.requestHeaders = [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestQueryOutput.TestQueryOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    messageDocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "message") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Com.Example.Model.TestQueryOutput.build $ do
        Com.Example.Model.TestQueryOutput.setMessage messageDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


