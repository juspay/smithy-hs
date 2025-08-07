module Com.Example.Command.TestReservedWords (
    TestReservedWordsError(..),
    testReservedWords
) where
import qualified Com.Example.ExampleServiceClient
import qualified Com.Example.Model.InternalServerError
import qualified Com.Example.Model.TestReservedWordsInput
import qualified Com.Example.Model.TestReservedWordsOutput
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
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types.Method
import qualified Network.HTTP.Types.URI

data TestReservedWordsError =
    InternalServerError Com.Example.Model.InternalServerError.InternalServerError
    | BuilderError Data.Text.Text
    | RequestError Data.Text.Text
       deriving (GHC.Generics.Generic, GHC.Show.Show)

instance Data.Aeson.ToJSON TestReservedWordsError
instance Data.Aeson.FromJSON TestReservedWordsError

serTestReservedWordsPAYLOAD:: Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput -> Network.HTTP.Client.RequestBody
serTestReservedWordsPAYLOAD input =
    Network.HTTP.Client.RequestBodyLBS $ Data.Aeson.encode $ Data.Aeson.object [
        "qualified" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.qualified' input,
        "instance" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.instance' input,
        "data" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.data' input,
        "import" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.import' input,
        "in" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.in' input,
        "module" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.module' input,
        "infixr" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.infixr' input,
        "do" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.do' input,
        "infix" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.infix' input,
        "then" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.then' input,
        "type" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.type' input,
        "newtype" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.newtype' input,
        "hiding" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.hiding' input,
        "as" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.as' input,
        "default" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.default' input,
        "deriving" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.deriving' input,
        "else" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.else' input,
        "infixl" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.infixl' input,
        "of" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.of' input,
        "let" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.let' input,
        "where" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.where' input,
        "class" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.class' input,
        "if" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.if' input,
        "case" Data.Aeson..= Com.Example.Model.TestReservedWordsInput.case' input
        ]
    

serTestReservedWordsLABEL :: Com.Example.Model.TestReservedWordsInput.TestReservedWordsInput -> Data.ByteString.ByteString
serTestReservedWordsLABEL input = 
    Data.ByteString.toStrict $ Data.ByteString.Builder.toLazyByteString $ Network.HTTP.Types.URI.encodePathSegmentsRelative [
        "reserved-words"
        ]
    

testReservedWords :: Com.Example.ExampleServiceClient.ExampleServiceClient -> Com.Example.Model.TestReservedWordsInput.TestReservedWordsInputBuilder () -> IO (Data.Either.Either TestReservedWordsError Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput)
testReservedWords client inputB = do
    let inputE = Com.Example.Model.TestReservedWordsInput.build inputB
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
        method = Network.HTTP.Types.Method.methodPost
        token = Data.Text.Encoding.encodeUtf8 $ Com.Example.ExampleServiceClient.token client
        toRequest input req =
            req {
                Network.HTTP.Client.path = serTestReservedWordsLABEL input
                , Network.HTTP.Client.method = method
                , Network.HTTP.Client.requestBody = serTestReservedWordsPAYLOAD input
                , Network.HTTP.Client.requestHeaders = [("Authorization", "Bearer " <> token)]
            }
        
    


deserializeResponse :: Network.HTTP.Client.Response Data.ByteString.Lazy.ByteString -> Data.Either.Either Data.Text.Text Com.Example.Model.TestReservedWordsOutput.TestReservedWordsOutput
deserializeResponse response = do
    
    responseObject :: Data.Aeson.Object <-
        Network.HTTP.Client.responseBody response
                Data.Function.& Data.Aeson.decode
                Data.Function.& Data.Maybe.maybe (Data.Either.Left "failed to parse response body") (Data.Either.Right)
        
    
    qualified'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "qualified") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    instance'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "instance") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    data'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "data") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    import'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "import") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    in'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "in") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    module'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "module") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    infixr'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "infixr") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    do'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "do") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    infix'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "infix") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    then'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "then") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    type'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "type") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    newtype'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "newtype") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    hiding'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "hiding") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    as'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "as") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    default'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "default") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    deriving'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "deriving") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    else'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "else") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    infixl'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "infixl") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    of'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "of") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    let'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "let") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    where'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "where") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    class'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "class") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    if'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "if") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    case'DocumentE :: Data.Text.Text <-
        Data.Aeson.Types.parseEither (flip (Data.Aeson..:) "case") responseObject
        Data.Function.& \case
            Data.Either.Left err -> Data.Either.Left (Data.Text.pack err)
            Data.Either.Right value -> Data.Either.Right value
        
    
    Com.Example.Model.TestReservedWordsOutput.build $ do
        Com.Example.Model.TestReservedWordsOutput.setQualified' qualified'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setInstance' instance'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setData' data'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setImport' import'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setIn' in'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setModule' module'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setInfixr' infixr'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setDo' do'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setInfix' infix'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setThen' then'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setType' type'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setNewtype' newtype'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setHiding' hiding'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setAs' as'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setDefault' default'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setDeriving' deriving'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setElse' else'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setInfixl' infixl'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setOf' of'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setLet' let'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setWhere' where'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setClass' class'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setIf' if'DocumentE
        Com.Example.Model.TestReservedWordsOutput.setCase' case'DocumentE
    
    where
        headers = Network.HTTP.Client.responseHeaders response
                    Data.Function.& Data.List.map (\(n, v) -> (Data.Text.Encoding.decodeUtf8 (Data.CaseInsensitive.original n), v))
        
        findHeader name = snd Data.Functor.<$> Data.List.find ((name ==) . fst) headers
        parseHeaderList :: Data.Aeson.FromJSON a => (Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text a) -> Data.ByteString.ByteString -> Data.Either.Either Data.Text.Text [a]
        parseHeaderList parser = sequence . Data.List.map (parser) . Data.ByteString.Char8.split ','
    


