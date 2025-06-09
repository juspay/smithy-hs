module Com.Example.Command.TestHttpLabels (
    TestHttpLabelsError(..),
    testHttpLabels
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpLabelsInput
import qualified Com.Example.Model.TestHttpLabelsOutput
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
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Network.HTTP.Client
import qualified Network.HTTP.Date
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data TestHttpLabelsError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serTestHttpLabelsLABEL :: Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput -> Data.ByteString.ByteString
serTestHttpLabelsLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "path_params",
        (Com.Example.Model.TestHttpLabelsInput.identifier input
                    Data.Function.& Com.Example.Utility.toRequestSegment)
        ,
        (Com.Example.Model.TestHttpLabelsInput.enabled input
                    Data.Function.& Com.Example.Utility.toRequestSegment)
        ,
        (Com.Example.Model.TestHttpLabelsInput.name input
                    Data.Function.& Com.Example.Utility.toRequestSegment)
        ,
        (Com.Example.Model.TestHttpLabelsInput.time input
                    Data.Function.& Com.Example.Utility.toRequestSegment)
        
        ]
    

testHttpLabels :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInputBuilder () -> IO (Data.Either.Either TestHttpLabelsError Com.Example.Model.TestHttpLabelsOutput.TestHttpLabelsOutput)
testHttpLabels client inputB = do
    let inputE = Com.Example.Model.TestHttpLabelsInput.build inputB
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
                Network.HTTP.Client.path = serTestHttpLabelsLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestHeaders = [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestHttpLabelsOutput.TestHttpLabelsOutput
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
        
    
    Com.Example.Model.TestHttpLabelsOutput.build $ do
        Com.Example.Model.TestHttpLabelsOutput.setMessage messageDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseTextHeader :: Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text Data.Text.Text
        parseTextHeader v = Data.Text.Encoding.decodeUtf8' v Data.Function.& \ case
            Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
            Data.Either.Right value -> Data.Either.Right value
        
        parseTimestampHeader :: Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text Network.HTTP.Date.HTTPDate
        parseTimestampHeader v = Data.Maybe.maybe (Data.Either.Left "failed to parse http datetime") (Data.Either.Right) $ Network.HTTP.Date.parseHTTPDate v
        parseHeader :: Data.Aeson.FromJSON a => Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a
        parseHeader v = Data.Aeson.eitherDecodeStrict v Data.Function.& \ case
            Data.Either.Left err -> Data.Either.Left $ Data.Text.pack $ show err
            Data.Either.Right value -> Data.Either.Right value
        
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


