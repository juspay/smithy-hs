{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Command.PostMenu (
    PostMenuError(..),
    postMenu
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.CoffeeItem
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.PostMenuInput
import qualified Com.Example.Model.PostMenuOutput
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
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data PostMenuError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text


serPostMenuPAYLOAD:: Com.Example.Model.PostMenuInput.PostMenuInput -> Network.HTTP.Client.RequestBody
serPostMenuPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "item" Data.Aeson..= Com.Example.Model.PostMenuInput.item input,
        "unionItem" Data.Aeson..= Com.Example.Model.PostMenuInput.unionItem input
        ]
    

serPostMenuQUERY :: Com.Example.Model.PostMenuInput.PostMenuInput -> Data.ByteString.ByteString
serPostMenuQUERY input =
    Network.HTTP.Types.URI.renderQuery True (Network.HTTP.Types.URI.queryTextToQuery m)
    where
        reservedParams = [
            "myQuery",
            "pageQuery"
            ]
        
        mapParams = Com.Example.Model.PostMenuInput.queryParams input
            Data.Function.& Data.Maybe.fromMaybe Data.Map.empty
            Data.Function.& Data.Map.toList
            Data.Function.& (Data.List.filter (\(k, _) -> not $ Data.List.any (== k) reservedParams))
            Data.Function.& (Data.List.map (\(k, v) -> (k, Data.Maybe.Just v)))
        
        staticParams = [
            ("myQuery", Data.Maybe.Just "123")
            ]
        
        dynamicParams = []
            ++ [("pageQuery", Data.Maybe.Just (Data.Text.pack $ show (Com.Example.Model.PostMenuInput.page input)))]
        
        m = staticParams ++ mapParams ++ dynamicParams
    

serPostMenuLABEL :: Com.Example.Model.PostMenuInput.PostMenuInput -> Data.ByteString.ByteString
serPostMenuLABEL input = 
    Data.Text.Encoding.encodeUtf8 _path
    where
        _path = Data.Text.empty
            <> "/menu"
            <> "/" <> (Com.Example.Model.PostMenuInput.some input)
        
    

postMenu :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.PostMenuInput.PostMenuInputBuilder () -> IO (Data.Either.Either PostMenuError Com.Example.Model.PostMenuOutput.PostMenuOutput)
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
            return $ Data.Bifunctor.first (RequestError) $ deserializeResponse response
        
    
    where
        method = Network.HTTP.Types.Method.methodPost
        toRequest input req =
            let path = (Network.HTTP.Client.path req) <> (serPostMenuLABEL input)
                in req {
                    Network.HTTP.Client.path = path
                    , Network.HTTP.Client.method = method
                    , Network.HTTP.Client.queryString = serPostMenuQUERY input
                    , Network.HTTP.Client.requestBody = serPostMenuPAYLOAD input
                }
            
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.PostMenuOutput.PostMenuOutput
deserializeResponse response = do
    config_tagHeaderE :: Data.Maybe.Maybe Data.Text.Text <-
        findHeader "x-config-tag"
        Data.Function.& Data.Maybe.maybe Data.Maybe.Nothing (Data.Aeson.decodeStrict)
        Data.Function.& Data.Either.Right
    
    resHeadersHeaderE :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) <- do
        filterHeaderByPrefix "x-some-header-"
        Data.Function.& Data.List.map (\(n, v) -> (stripPrefix "x-some-header-" n, Data.Text.Encoding.decodeUtf8 v))
        Data.Function.& Data.Map.fromList
        Data.Function.& Data.Maybe.Just
        Data.Function.& Data.Either.Right
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
        Data.Function.& Data.Aeson.decode
        Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
    
    itemsDocumentE :: Com.Example.Model.CoffeeItem.CoffeeItem <-
        Data.Aeson.Types.parseEither (flip (undefined) "items") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Com.Example.Model.PostMenuOutput.build $ do
        Com.Example.Model.PostMenuOutput.setConfigTag config_tagHeaderE
        Com.Example.Model.PostMenuOutput.setResheaders resHeadersHeaderE
        Com.Example.Model.PostMenuOutput.setItems itemsDocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
            Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        filterHeaderByPrefix prefix = Data.List.filter (Data.Text.isPrefixOf prefix . fst) headers
        stripPrefix prefix s = Data.Maybe.maybe s (id) $ Data.Text.stripPrefix prefix s
    


