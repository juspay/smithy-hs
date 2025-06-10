module Com.Example.Model.TestHttpPayloadInput (
    setPayload,
    setIdentifier,
    setStringheader,
    setPrefixheaders,
    build,
    TestHttpPayloadInputBuilder,
    TestHttpPayloadInput,
    payload,
    identifier,
    stringHeader,
    prefixHeaders
) where
import qualified Com.Example.Model.CoffeeItem
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpPayloadInput = TestHttpPayloadInput {
    payload :: Com.Example.Model.CoffeeItem.CoffeeItem,
    identifier :: Integer,
    stringHeader :: Data.Maybe.Maybe Data.Text.Text,
    prefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpPayloadInput where
    toJSON a = Data.Aeson.object [
        "payload" Data.Aeson..= payload a,
        "identifier" Data.Aeson..= identifier a,
        "stringHeader" Data.Aeson..= stringHeader a,
        "prefixHeaders" Data.Aeson..= prefixHeaders a
        ]
    


instance Data.Aeson.FromJSON TestHttpPayloadInput where
    parseJSON = Data.Aeson.withObject "TestHttpPayloadInput" $ \v -> TestHttpPayloadInput
        Data.Functor.<$> (v Data.Aeson..: "payload")
        Control.Applicative.<*> (v Data.Aeson..: "identifier")
        Control.Applicative.<*> (v Data.Aeson..: "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..: "prefixHeaders")
    



data TestHttpPayloadInputBuilderState = TestHttpPayloadInputBuilderState {
    payloadBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    identifierBuilderState :: Data.Maybe.Maybe Integer,
    stringHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    prefixHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpPayloadInputBuilderState
defaultBuilderState = TestHttpPayloadInputBuilderState {
    payloadBuilderState = Data.Maybe.Nothing,
    identifierBuilderState = Data.Maybe.Nothing,
    stringHeaderBuilderState = Data.Maybe.Nothing,
    prefixHeadersBuilderState = Data.Maybe.Nothing
}

newtype TestHttpPayloadInputBuilder a = TestHttpPayloadInputBuilder {
    runTestHttpPayloadInputBuilder :: TestHttpPayloadInputBuilderState -> (TestHttpPayloadInputBuilderState, a)
}

instance Data.Functor.Functor TestHttpPayloadInputBuilder where
    fmap f (TestHttpPayloadInputBuilder g) =
        TestHttpPayloadInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpPayloadInputBuilder where
    pure a = TestHttpPayloadInputBuilder (\s -> (s, a))
    (TestHttpPayloadInputBuilder f) <*> (TestHttpPayloadInputBuilder g) = TestHttpPayloadInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpPayloadInputBuilder where
    (TestHttpPayloadInputBuilder f) >>= g = TestHttpPayloadInputBuilder (\s ->
        let (s', a) = f s
            (TestHttpPayloadInputBuilder h) = g a
        in h s')

setPayload :: Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpPayloadInputBuilder ()
setPayload value =
   TestHttpPayloadInputBuilder (\s -> (s { payloadBuilderState = Data.Maybe.Just value }, ()))

setIdentifier :: Integer -> TestHttpPayloadInputBuilder ()
setIdentifier value =
   TestHttpPayloadInputBuilder (\s -> (s { identifierBuilderState = Data.Maybe.Just value }, ()))

setStringheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpPayloadInputBuilder ()
setStringheader value =
   TestHttpPayloadInputBuilder (\s -> (s { stringHeaderBuilderState = value }, ()))

setPrefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpPayloadInputBuilder ()
setPrefixheaders value =
   TestHttpPayloadInputBuilder (\s -> (s { prefixHeadersBuilderState = value }, ()))

build :: TestHttpPayloadInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadInput
build builder = do
    let (st, _) = runTestHttpPayloadInputBuilder builder defaultBuilderState
    payload' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpPayloadInput.TestHttpPayloadInput.payload is a required property.") Data.Either.Right (payloadBuilderState st)
    identifier' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpPayloadInput.TestHttpPayloadInput.identifier is a required property.") Data.Either.Right (identifierBuilderState st)
    stringHeader' <- Data.Either.Right (stringHeaderBuilderState st)
    prefixHeaders' <- Data.Either.Right (prefixHeadersBuilderState st)
    Data.Either.Right (TestHttpPayloadInput { 
        payload = payload',
        identifier = identifier',
        stringHeader = stringHeader',
        prefixHeaders = prefixHeaders'
    })


