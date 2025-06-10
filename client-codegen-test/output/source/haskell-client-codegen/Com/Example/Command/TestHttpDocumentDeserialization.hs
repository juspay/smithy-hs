module Com.Example.Command.TestHttpDocumentDeserialization (
    TestHttpDocumentDeserializationError(..),
    testHttpDocumentDeserialization
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.CoffeeCustomization
import qualified Com.Example.Model.CoffeeItem
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpDocumentDeserializationInput
import qualified Com.Example.Model.TestHttpDocumentDeserializationOutput
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
import qualified Network.HTTP.Client
import qualified Network.HTTP.Date
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data TestHttpDocumentDeserializationError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serTestHttpDocumentDeserializationQUERY :: Com.Example.Model.TestHttpDocumentDeserializationInput.TestHttpDocumentDeserializationInput -> Data.ByteString.ByteString
serTestHttpDocumentDeserializationQUERY input =
    let
        staticParams = [
            ]
        
        coffeeTypeQuery = Com.Example.Model.TestHttpDocumentDeserializationInput.coffeeType input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (Com.Example.Utility.toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("type", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ coffeeTypeQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serTestHttpDocumentDeserializationLABEL :: Com.Example.Model.TestHttpDocumentDeserializationInput.TestHttpDocumentDeserializationInput -> Data.ByteString.ByteString
serTestHttpDocumentDeserializationLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "document_response"
        ]
    

testHttpDocumentDeserialization :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpDocumentDeserializationInput.TestHttpDocumentDeserializationInputBuilder () -> IO (Data.Either.Either TestHttpDocumentDeserializationError Com.Example.Model.TestHttpDocumentDeserializationOutput.TestHttpDocumentDeserializationOutput)
testHttpDocumentDeserialization client inputB = do
    let inputE = Com.Example.Model.TestHttpDocumentDeserializationInput.build inputB
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
                Network.HTTP.Client.path = serTestHttpDocumentDeserializationLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serTestHttpDocumentDeserializationQUERY input
                , Network.HTTP.Client.requestHeaders = [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestHttpDocumentDeserializationOutput.TestHttpDocumentDeserializationOutput
deserializeResponse response = do
    outputHeaderHeaderE :: Data.Maybe.Maybe Data.Text.Text <-
        (findHeader "x-output-header" Data.Functor.<&> Com.Example.Utility.fromResponseSegment)
                Data.Function.& sequence
        
    
    outputHeaderBoolHeaderE :: Data.Maybe.Maybe Bool <-
        (findHeader "x-output-header-bool" Data.Functor.<&> Com.Example.Utility.fromResponseSegment)
                Data.Function.& sequence
        
    
    outputHeaderListHeaderE :: Data.Maybe.Maybe ([] Data.Text.Text) <-
        (findHeader "x-output-header-list" Data.Functor.<&> parseHeaderList Com.Example.Utility.fromResponseSegment)
                Data.Function.& sequence
        
    
    outputHeaderIntHeaderE :: Data.Maybe.Maybe Integer <-
        (findHeader "x-output-header-int" Data.Functor.<&> Com.Example.Utility.fromResponseSegment)
                Data.Function.& sequence
        
    
    outputPrefixHeadersHeaderE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) <- do
        filterHeaderByPrefix "x-output-prefix-"
                Data.Function.& Data.List.map (\(n, v) -> (stripPrefix "x-output-prefix-" n, Data.Text.Encoding.decodeUtf8 v))
                Data.Function.& Data.Map.fromList
                Data.Function.& Data.Maybe.Just
                Data.Function.& Data.Either.Right
        
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    itemDocumentE :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "item") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    customizationDocumentE :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "customization") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    timeDocumentE :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "time") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Com.Example.Model.TestHttpDocumentDeserializationOutput.build $ do
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setOutputheader outputHeaderHeaderE
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setOutputheaderbool outputHeaderBoolHeaderE
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setOutputheaderlist outputHeaderListHeaderE
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setOutputheaderint outputHeaderIntHeaderE
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setOutputprefixheaders outputPrefixHeadersHeaderE
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setItem itemDocumentE
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setCustomization customizationDocumentE
        Com.Example.Model.TestHttpDocumentDeserializationOutput.setTime timeDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
        filterHeaderByPrefix prefix = Data.List.filter (Data.Text.isPrefixOf prefix . fst) headers
        stripPrefix prefix s = Data.Maybe.maybe s (id) $ Data.Text.stripPrefix prefix s
    


