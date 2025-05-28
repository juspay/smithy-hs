{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Command.GetMenu (
    GetMenuError(..),
    getMenu
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.CoffeeItem
import qualified Com.Example.Model.GetMenuInput
import qualified Com.Example.Model.GetMenuOutput
import qualified Com.Example.Model.InternalServerError
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
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Method

data GetMenuError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serGetMenuLABEL :: Com.Example.Model.GetMenuInput.GetMenuInput -> Data.ByteString.ByteString
serGetMenuLABEL input = 
    Data.Text.Encoding.encodeUtf8 _path
    where
        _path = Data.Text.empty
            <> "/menu"
        
    

getMenu :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.GetMenuInput.GetMenuInputBuilder () -> IO (Data.Either.Either GetMenuError Com.Example.Model.GetMenuOutput.GetMenuOutput)
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
            return $ Data.Bifunctor.first (RequestError) $ deserializeResponse response
        
    
    where
        method = Network.HTTP.Types.Method.methodGet
        toRequest input req =
            let path = (Network.HTTP.Client.path req) <> (serGetMenuLABEL input)
                in req {
                    Network.HTTP.Client.path = path
                    , Network.HTTP.Client.method = method
                }
            
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.GetMenuOutput.GetMenuOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
        Data.Function.& Data.Aeson.decode
        Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
    
    itemsDocumentE :: Data.Maybe.Maybe ([] Com.Example.Model.CoffeeItem.CoffeeItem) <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:?) "items") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Com.Example.Model.GetMenuOutput.build $ do
        Com.Example.Model.GetMenuOutput.setItems itemsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
            Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
    


