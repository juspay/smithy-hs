module Com.Example.Model.TestHttpDocumentInput (
    setPayload,
    setCustomization,
    setTime,
    setIdentifier,
    setStringheader,
    setPrefixheaders,
    build,
    TestHttpDocumentInputBuilder,
    TestHttpDocumentInput,
    payload,
    customization,
    time,
    identifier,
    stringHeader,
    prefixHeaders
) where
import qualified Com.Example.Model.CoffeeCustomization
import qualified Com.Example.Model.CoffeeItem
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Date

data TestHttpDocumentInput = TestHttpDocumentInput {
    payload :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
    time :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    identifier :: Integer,
    stringHeader :: Data.Maybe.Maybe Data.Text.Text,
    prefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpDocumentInput where
    toJSON a = Data.Aeson.object [
        "payload" Data.Aeson..= payload a,
        "customization" Data.Aeson..= customization a,
        "time" Data.Aeson..= ((time a) Data.Functor.<&> (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate)),
        "identifier" Data.Aeson..= identifier a,
        "stringHeader" Data.Aeson..= stringHeader a,
        "prefixHeaders" Data.Aeson..= prefixHeaders a
        ]
    


instance Data.Aeson.FromJSON TestHttpDocumentInput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentInput" $ \v -> TestHttpDocumentInput
        Data.Functor.<$> (v Data.Aeson..: "payload")
        Control.Applicative.<*> (v Data.Aeson..: "customization")
        Control.Applicative.<*> (v Data.Aeson..: "time"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..: "identifier")
        Control.Applicative.<*> (v Data.Aeson..: "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..: "prefixHeaders")
    



data TestHttpDocumentInputBuilderState = TestHttpDocumentInputBuilderState {
    payloadBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customizationBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    identifierBuilderState :: Data.Maybe.Maybe Integer,
    stringHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    prefixHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpDocumentInputBuilderState
defaultBuilderState = TestHttpDocumentInputBuilderState {
    payloadBuilderState = Data.Maybe.Nothing,
    customizationBuilderState = Data.Maybe.Nothing,
    timeBuilderState = Data.Maybe.Nothing,
    identifierBuilderState = Data.Maybe.Nothing,
    stringHeaderBuilderState = Data.Maybe.Nothing,
    prefixHeadersBuilderState = Data.Maybe.Nothing
}

newtype TestHttpDocumentInputBuilder a = TestHttpDocumentInputBuilder {
    runTestHttpDocumentInputBuilder :: TestHttpDocumentInputBuilderState -> (TestHttpDocumentInputBuilderState, a)
}

instance Data.Functor.Functor TestHttpDocumentInputBuilder where
    fmap f (TestHttpDocumentInputBuilder g) =
        TestHttpDocumentInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpDocumentInputBuilder where
    pure a = TestHttpDocumentInputBuilder (\s -> (s, a))
    (TestHttpDocumentInputBuilder f) <*> (TestHttpDocumentInputBuilder g) = TestHttpDocumentInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpDocumentInputBuilder where
    (TestHttpDocumentInputBuilder f) >>= g = TestHttpDocumentInputBuilder (\s ->
        let (s', a) = f s
            (TestHttpDocumentInputBuilder h) = g a
        in h s')

setPayload :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpDocumentInputBuilder ()
setPayload value =
   TestHttpDocumentInputBuilder (\s -> (s { payloadBuilderState = value }, ()))

setCustomization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization -> TestHttpDocumentInputBuilder ()
setCustomization value =
   TestHttpDocumentInputBuilder (\s -> (s { customizationBuilderState = value }, ()))

setTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> TestHttpDocumentInputBuilder ()
setTime value =
   TestHttpDocumentInputBuilder (\s -> (s { timeBuilderState = value }, ()))

setIdentifier :: Integer -> TestHttpDocumentInputBuilder ()
setIdentifier value =
   TestHttpDocumentInputBuilder (\s -> (s { identifierBuilderState = Data.Maybe.Just value }, ()))

setStringheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpDocumentInputBuilder ()
setStringheader value =
   TestHttpDocumentInputBuilder (\s -> (s { stringHeaderBuilderState = value }, ()))

setPrefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpDocumentInputBuilder ()
setPrefixheaders value =
   TestHttpDocumentInputBuilder (\s -> (s { prefixHeadersBuilderState = value }, ()))

build :: TestHttpDocumentInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentInput
build builder = do
    let (st, _) = runTestHttpDocumentInputBuilder builder defaultBuilderState
    payload' <- Data.Either.Right (payloadBuilderState st)
    customization' <- Data.Either.Right (customizationBuilderState st)
    time' <- Data.Either.Right (timeBuilderState st)
    identifier' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInput.identifier is a required property.") Data.Either.Right (identifierBuilderState st)
    stringHeader' <- Data.Either.Right (stringHeaderBuilderState st)
    prefixHeaders' <- Data.Either.Right (prefixHeadersBuilderState st)
    Data.Either.Right (TestHttpDocumentInput { 
        payload = payload',
        customization = customization',
        time = time',
        identifier = identifier',
        stringHeader = stringHeader',
        prefixHeaders = prefixHeaders'
    })


