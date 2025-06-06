module Com.Example.Command.TestHttpPayload (
    TestHttpPayloadError(..),
    testHttpPayload
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpPayloadInput
import qualified Com.Example.Model.TestHttpPayloadOutput
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
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data TestHttpPayloadError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


class RequestSegment a where
    toRequestSegment :: Show a => a -> Data.Text.Text
instance RequestSegment Data.Text.Text where
    toRequestSegment = id
instance RequestSegment Integer where
    toRequestSegment = Data.Text.pack . show
instance RequestSegment Bool where
    toRequestSegment = Data.Text.toLower . Data.Text.pack . show
instance RequestSegment Network.HTTP.Date.HTTPDate where
    toRequestSegment = Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate

serTestHttpPayloadPAYLOAD:: Com.Example.Model.TestHttpPayloadInput.TestHttpPayloadInput -> Network.HTTP.Client.RequestBody
serTestHttpPayloadPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $
    Data.Aeson.encode $
    Com.Example.Model.TestHttpPayloadInput.payload input

serTestHttpPayloadHEADER :: Com.Example.Model.TestHttpPayloadInput.TestHttpPayloadInput -> Network.HTTP.Types.Header.RequestHeaders
serTestHttpPayloadHEADER input =
    let 
        stringHeaderHeader = (Com.Example.Model.TestHttpPayloadInput.stringHeader input
                    Data.Functor.<&> toRequestSegment)
        
                    Data.Functor.<&> \x -> [("x-header-string", Data.Text.Encoding.encodeUtf8 x)]
        
        prefixHeadersHeader = Com.Example.Model.TestHttpPayloadInput.prefixHeaders input
                    Data.Functor.<&> Data.Map.toList
                    Data.Functor.<&> Data.List.map (\(n, v) -> (toHeaderName "x-prefix-" n, Data.Text.Encoding.encodeUtf8 v))
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            stringHeaderHeader,
            prefixHeadersHeader
            ]
        
    
    where
        toHeaderName prefix name = Data.CaseInsensitive.mk $ Data.Text.Encoding.encodeUtf8 $ prefix <> name
    

serTestHttpPayloadLABEL :: Com.Example.Model.TestHttpPayloadInput.TestHttpPayloadInput -> Data.ByteString.ByteString
serTestHttpPayloadLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "payload",
        (Com.Example.Model.TestHttpPayloadInput.identifier input
                    Data.Function.& toRequestSegment)
        
        ]
    

testHttpPayload :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpPayloadInput.TestHttpPayloadInputBuilder () -> IO (Data.Either.Either TestHttpPayloadError Com.Example.Model.TestHttpPayloadOutput.TestHttpPayloadOutput)
testHttpPayload client inputB = do
    let inputE = Com.Example.Model.TestHttpPayloadInput.build inputB
        baseUri = Com.Example.ExampleServiceClient.endpointUri client
        token = Com.Example.ExampleServiceClient.token client
        httpManager = Com.Example.ExampleServiceClient.httpManager client
        requestE = Network.HTTP.Client.requestFromURI @(Data.Either.Either Control.Exception.SomeException) baseUri
    
    case (inputE, requestE) of
        (Data.Either.Left err, _) -> return $ Data.Either.Left (BuilderError err)
        (_, Data.Either.Left err) -> return $ Data.Either.Left (RequestError $ Data.Text.pack $ show err)
        (Data.Either.Right input, Data.Either.Right req) -> do
            response <- Network.HTTP.Client.httpLbs (toRequest input req) httpManager
            return $ Data.Bifunctor.first (RequestError) $ deserializeResponse response
        
    
    where
        method = Network.HTTP.Types.Method.methodPost
        toRequest input req =
            req {
                Network.HTTP.Client.path = serTestHttpPayloadLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serTestHttpPayloadPAYLOAD input
                , Network.HTTP.Client.requestHeaders = serTestHttpPayloadHEADER input
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestHttpPayloadOutput.TestHttpPayloadOutput
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
        
    
    Com.Example.Model.TestHttpPayloadOutput.build $ do
        Com.Example.Model.TestHttpPayloadOutput.setMessage messageDocumentE
    
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
    


