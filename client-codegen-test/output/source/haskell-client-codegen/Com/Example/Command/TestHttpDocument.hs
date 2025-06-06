module Com.Example.Command.TestHttpDocument (
    TestHttpDocumentError(..),
    testHttpDocument
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpDocumentInput
import qualified Com.Example.Model.TestHttpDocumentOutput
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

data TestHttpDocumentError =
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

serTestHttpDocumentPAYLOAD:: Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInput -> Network.HTTP.Client.RequestBody
serTestHttpDocumentPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "payload" Data.Aeson..= Com.Example.Model.TestHttpDocumentInput.payload input,
        "customization" Data.Aeson..= Com.Example.Model.TestHttpDocumentInput.customization input,
        "time" Data.Aeson..= ((Com.Example.Model.TestHttpDocumentInput.time input) Data.Functor.<&> (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate))
        ]
    

serTestHttpDocumentHEADER :: Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInput -> Network.HTTP.Types.Header.RequestHeaders
serTestHttpDocumentHEADER input =
    let 
        stringHeaderHeader = (Com.Example.Model.TestHttpDocumentInput.stringHeader input
                    Data.Functor.<&> toRequestSegment)
        
                    Data.Functor.<&> \x -> [("x-header-string", Data.Text.Encoding.encodeUtf8 x)]
        
        prefixHeadersHeader = Com.Example.Model.TestHttpDocumentInput.prefixHeaders input
                    Data.Functor.<&> Data.Map.toList
                    Data.Functor.<&> Data.List.map (\(n, v) -> (toHeaderName "x-prefix-" n, Data.Text.Encoding.encodeUtf8 v))
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            stringHeaderHeader,
            prefixHeadersHeader
            ]
        
    
    where
        toHeaderName prefix name = Data.CaseInsensitive.mk $ Data.Text.Encoding.encodeUtf8 $ prefix <> name
    

serTestHttpDocumentLABEL :: Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInput -> Data.ByteString.ByteString
serTestHttpDocumentLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "document",
        (Com.Example.Model.TestHttpDocumentInput.identifier input
                    Data.Function.& toRequestSegment)
        
        ]
    

testHttpDocument :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInputBuilder () -> IO (Data.Either.Either TestHttpDocumentError Com.Example.Model.TestHttpDocumentOutput.TestHttpDocumentOutput)
testHttpDocument client inputB = do
    let inputE = Com.Example.Model.TestHttpDocumentInput.build inputB
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
                Network.HTTP.Client.path = serTestHttpDocumentLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serTestHttpDocumentPAYLOAD input
                , Network.HTTP.Client.requestHeaders = serTestHttpDocumentHEADER input
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestHttpDocumentOutput.TestHttpDocumentOutput
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
        
    
    Com.Example.Model.TestHttpDocumentOutput.build $ do
        Com.Example.Model.TestHttpDocumentOutput.setMessage messageDocumentE
    
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
    


