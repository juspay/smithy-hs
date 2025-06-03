{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Command.TestHttpPayloadDeserialization (
    TestHttpPayloadDeserializationError(..),
    testHttpPayloadDeserialization
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.CoffeeItem
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestHttpPayloadDeserializationInput
import qualified Com.Example.Model.TestHttpPayloadDeserializationOutput
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Bifunctor
import qualified Data.ByteString
import qualified Data.ByteString.Builder
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
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data TestHttpPayloadDeserializationError =
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

serTestHttpPayloadDeserializationQUERY :: Com.Example.Model.TestHttpPayloadDeserializationInput.TestHttpPayloadDeserializationInput -> Data.ByteString.ByteString
serTestHttpPayloadDeserializationQUERY input =
    let
        staticParams = [
            ]
        
        coffeeTypeQuery = Com.Example.Model.TestHttpPayloadDeserializationInput.coffeeType input
                    Data.Functor.<&> (\x -> [x])
                    Data.Functor.<&> Data.List.map (toRequestSegment)
                    Data.Functor.<&> Data.List.map (\x -> toQueryItem ("type", x))
                    Data.Function.& Data.Maybe.maybe [] (id)
        
        m = staticParams ++ coffeeTypeQuery
        in Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    
    where
        toQueryItem (k, v) = (k, Data.Maybe.Just v)
        toQuery = Data.List.map (toQueryItem)
        expandTuple (key, values) = Data.List.map (\v -> (key, v)) values
    

serTestHttpPayloadDeserializationLABEL :: Com.Example.Model.TestHttpPayloadDeserializationInput.TestHttpPayloadDeserializationInput -> Data.ByteString.ByteString
serTestHttpPayloadDeserializationLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "payload_response"
        ]
    

testHttpPayloadDeserialization :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestHttpPayloadDeserializationInput.TestHttpPayloadDeserializationInputBuilder () -> IO (Data.Either.Either TestHttpPayloadDeserializationError Com.Example.Model.TestHttpPayloadDeserializationOutput.TestHttpPayloadDeserializationOutput)
testHttpPayloadDeserialization client inputB = do
    let inputE = Com.Example.Model.TestHttpPayloadDeserializationInput.build inputB
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
            req {
                Network.HTTP.Client.path = serTestHttpPayloadDeserializationLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.queryString = serTestHttpPayloadDeserializationQUERY input
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestHttpPayloadDeserializationOutput.TestHttpPayloadDeserializationOutput
deserializeResponse response = do
    outputHeaderHeaderE :: Data.Maybe.Maybe Data.Text.Text <-
        (findHeader "x-output-header" Control.Monad.>>= Data.Aeson.decodeStrict)
                Data.Function.& Data.Either.Right
        
    
    outputHeaderBoolHeaderE :: Data.Maybe.Maybe Bool <-
        (findHeader "x-output-header-bool" Control.Monad.>>= Data.Aeson.decodeStrict)
                Data.Function.& Data.Either.Right
        
    
    outputHeaderListHeaderE :: Data.Maybe.Maybe ([] Data.Text.Text) <-
        (findHeader "x-output-header-list" Control.Monad.>>= Data.Aeson.decodeStrict)
                Data.Function.& Data.Either.Right
        
    
    outputHeaderIntHeaderE :: Data.Maybe.Maybe Integer <-
        (findHeader "x-output-header-int" Control.Monad.>>= Data.Aeson.decodeStrict)
                Data.Function.& Data.Either.Right
        
    
    outputPrefixHeadersHeaderE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) <- do
        filterHeaderByPrefix "x-output-prefix-"
                Data.Function.& Data.List.map (\(n, v) -> (stripPrefix "x-output-prefix-" n, Data.Text.Encoding.decodeUtf8 v))
                Data.Function.& Data.Map.fromList
                Data.Function.& Data.Maybe.Just
                Data.Function.& Data.Either.Right
        
    
    itemPayloadE :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem <- do
        Data.Aeson.decode (Network.HTTP.Client.responseBody response)
                Data.Function.& Data.Either.Right
        
    
    Com.Example.Model.TestHttpPayloadDeserializationOutput.build $ do
        Com.Example.Model.TestHttpPayloadDeserializationOutput.setOutputheader outputHeaderHeaderE
        Com.Example.Model.TestHttpPayloadDeserializationOutput.setOutputheaderbool outputHeaderBoolHeaderE
        Com.Example.Model.TestHttpPayloadDeserializationOutput.setOutputheaderlist outputHeaderListHeaderE
        Com.Example.Model.TestHttpPayloadDeserializationOutput.setOutputheaderint outputHeaderIntHeaderE
        Com.Example.Model.TestHttpPayloadDeserializationOutput.setOutputprefixheaders outputPrefixHeadersHeaderE
        Com.Example.Model.TestHttpPayloadDeserializationOutput.setItem itemPayloadE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
            Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        filterHeaderByPrefix prefix = Data.List.filter (Data.Text.isPrefixOf prefix . fst) headers
        stripPrefix prefix s = Data.Maybe.maybe s (id) $ Data.Text.stripPrefix prefix s
    


