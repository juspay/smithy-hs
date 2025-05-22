{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Command.PostMenu (
    postMenu
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.PostMenuInput
import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.ByteString
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

-- Operation Error
data PostMenuError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


postMenu :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.PostMenuInput.PostMenuInputBuilder () -> IO (Data.Either.Either PostMenuError ())
postMenu client inputB = do
    let inputE = Com.Example.Model.PostMenuInput.build inputB
        baseUri = Com.Example.ExampleServiceClient.endpointUri client
        token = Com.Example.ExampleServiceClient.token client
        httpManager = Com.Example.ExampleServiceClient.httpManager client
        requestE = Network.HTTP.Client.requestFromURI @(Data.Either.Either Control.Exception.SomeException) baseUri
    
    case (inputE, requestE) of
        (Data.Either.Left err, _) -> return $ Data.Either.Left (BuilderError err)
        (_, Data.Either.Left err) -> return $ Data.Either.Left (RequestError $ Data.Text.pack $ show err)
        (Data.Either.Right input, Data.Either.Right req) -> do
            response <- Network.HTTP.Client.httpLbs (toRequest input req) httpManager
            return (Data.Either.Right ())
        
    
    where
        method = Network.HTTP.Types.Method.methodPost
        toRequest input req =
            let path = (Network.HTTP.Client.path req) <> (requestPath input)
                in req {
                    Network.HTTP.Client.path = path,
                    Network.HTTP.Client.method = method,
                    Network.HTTP.Client.queryString = requestQuery input,
                    Network.HTTP.Client.requestBody = requestPayload input
                }
            
        
    


requestPayload :: Com.Example.Model.PostMenuInput.PostMenuInput -> Network.HTTP.Client.RequestBody
requestPayload input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object
        [ "dateTime" Data.Aeson..= Com.Example.Model.PostMenuInput.dateTime input
        , "item" Data.Aeson..= Com.Example.Model.PostMenuInput.item input
        , "unionItem" Data.Aeson..= Com.Example.Model.PostMenuInput.unionItem input
        , "document" Data.Aeson..= Com.Example.Model.PostMenuInput.document input
        , "epoch" Data.Aeson..= Com.Example.Model.PostMenuInput.epoch input
        , "httpDateTime" Data.Aeson..= ((Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate) Data.Functor.<$> Com.Example.Model.PostMenuInput.httpDateTime input)
        ]
    


requestQuery :: Com.Example.Model.PostMenuInput.PostMenuInput -> Data.ByteString.ByteString
requestQuery input =
    Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    where
        reservedParams =
            [ "myQuery"
            , "pageQuery"
            ]
        
        mapParams = Com.Example.Model.PostMenuInput.queryParams input
            Data.Function.& Data.Maybe.fromMaybe Data.Map.empty
            Data.Function.& Data.Map.toList
            Data.Function.& (Data.List.filter (\(k, _) -> not $ Data.List.any (== k) reservedParams))
            Data.Function.& (Data.List.map (\(k, v) -> (k, Data.Maybe.Just v)))
        
        staticParams = []
            ++[("myQuery", Data.Maybe.Just "123")]
        
        dynamicParams = []
            ++ [("pageQuery", Data.Maybe.Just (Data.Text.pack $ show (Com.Example.Model.PostMenuInput.page input)))]
        
        m = staticParams ++ mapParams ++ dynamicParams
    


requestPath :: Com.Example.Model.PostMenuInput.PostMenuInput -> Data.ByteString.ByteString
requestPath input = 
    Data.Text.Encoding.encodeUtf8 _path
    where
        _path = Data.Text.empty
            <> "menu"
            <> (Com.Example.Model.PostMenuInput.some input)
        
    


