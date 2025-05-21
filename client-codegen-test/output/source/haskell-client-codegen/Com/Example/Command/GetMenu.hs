{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Command.GetMenu (
    getMenu
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.GetMenuInput
import qualified Com.Example.Model.InternalServerError
import qualified Control.Exception
import qualified Data.Aeson
import qualified Data.ByteString
import qualified Data.Either
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

-- Operation Error
data GetMenuError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


getMenu :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.GetMenuInput.GetMenuInputBuilder () -> IO (Data.Either.Either GetMenuError ())
getMenu client inputB = do
    let inputE = Com.Example.Model.GetMenuInput.build inputB
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
        method = Network.HTTP.Types.Method.methodGet
        toRequest input req =
            let path = (Network.HTTP.Client.path req) <> (requestPath input)
                in req {
                    Network.HTTP.Client.path = path,
                    Network.HTTP.Client.method = method,
                    Network.HTTP.Client.queryString = requestQuery input,
                    Network.HTTP.Client.requestBody = requestPayload input
                }
            
        
    


requestPayload :: Com.Example.Model.GetMenuInput.GetMenuInput -> Network.HTTP.Client.RequestBody
requestPayload input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object
        []


requestQuery :: Com.Example.Model.GetMenuInput.GetMenuInput -> Data.ByteString.ByteString
requestQuery input =
    Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    where
        staticParams = []
        
        dynamicParams = []
        
        m = staticParams ++ dynamicParams
    


requestPath :: Com.Example.Model.GetMenuInput.GetMenuInput -> Data.ByteString.ByteString
requestPath input = 
    Data.Text.Encoding.encodeUtf8 _path
    where
        _path = Data.Text.empty
            <> "menu"
        
    


