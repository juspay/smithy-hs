{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Command.TestHttpHeaders (
    TestHttpHeadersError(..),
    testHttpHeaders
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpHeadersInput
import qualified Com.Example.Model.TestHttpHeadersOutput
import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.Aeson.Types
import qualified Data.Bifunctor
import qualified Data.ByteString
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
import qualified Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Method

data TestHttpHeadersError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serTestHttpHeadersHEADER :: Com.Example.Model.TestHttpHeadersInput.TestHttpHeadersInput -> Network.HTTP.Types.Header.RequestHeaders
serTestHttpHeadersHEADER input =
    let 
        boolHeaderHeader = (Com.Example.Model.TestHttpHeadersInput.boolHeader input)
                    Data.Functor.<&> \x -> [("x-header-bool", Data.Text.Encoding.encodeUtf8 x)]
        
        intHeaderHeader = (Com.Example.Model.TestHttpHeadersInput.intHeader input)
                    Data.Functor.<&> \x -> [("x-header-int", Data.Text.Encoding.encodeUtf8 x)]
        
        listHeaderHeader = (Com.Example.Model.TestHttpHeadersInput.listHeader input)
                    Data.Functor.<&> \x -> [("x-header-list", Data.Text.Encoding.encodeUtf8 x)]
        
        stringHeaderHeader = (Com.Example.Model.TestHttpHeadersInput.stringHeader input)
                    Data.Functor.<&> \x -> [("x-header-string", Data.Text.Encoding.encodeUtf8 x)]
        
        prefixHeadersHeader = Com.Example.Model.TestHttpHeadersInput.prefixHeaders input
                    Data.Functor.<&> Data.Map.toList
                    Data.Functor.<&> Data.List.map (\(n, v) -> (toHeaderName "x-prefix-" n, Data.Text.Encoding.encodeUtf8 v))
        
        in Data.List.concat $ Data.Maybe.catMaybes [
            boolHeaderHeader,
            intHeaderHeader,
            listHeaderHeader,
            stringHeaderHeader,
            prefixHeadersHeader
            ]
        
    
    where
        toHeaderName prefix name = Data.CaseInsensitive.mk $ Data.Text.Encoding.encodeUtf8 $ prefix <> name
    

serTestHttpHeadersLABEL :: Com.Example.Model.TestHttpHeadersInput.TestHttpHeadersInput -> Data.ByteString.ByteString
serTestHttpHeadersLABEL input = 
    Data.Text.Encoding.encodeUtf8 _path
    where
        _path = Data.Text.empty
            <> "/headers"
        
    

testHttpHeaders :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpHeadersInput.TestHttpHeadersInputBuilder () -> IO (Data.Either.Either TestHttpHeadersError Com.Example.Model.TestHttpHeadersOutput.TestHttpHeadersOutput)
testHttpHeaders client inputB = do
    let inputE = Com.Example.Model.TestHttpHeadersInput.build inputB
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
        method = Network.HTTP.Types.Method.methodGet
        toRequest input req =
            let path = (Network.HTTP.Client.path req) <> (serTestHttpHeadersLABEL input)
                in req {
                    Network.HTTP.Client.path = path
                    , Network.HTTP.Client.method = method
                    , Network.HTTP.Client.requestHeaders = serTestHttpHeadersHEADER input
                }
            
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestHttpHeadersOutput.TestHttpHeadersOutput
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
        
    
    Com.Example.Model.TestHttpHeadersOutput.build $ do
        Com.Example.Model.TestHttpHeadersOutput.setMessage messageDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
            Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
    


