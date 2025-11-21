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
import qualified Com.Example.Utility
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Types.Method

data TestHttpPayloadInput = TestHttpPayloadInput {
    payload :: Com.Example.Model.CoffeeItem.CoffeeItem,
    identifier :: Data.Int.Int32,
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
    

instance Com.Example.Utility.SerializeBody TestHttpPayloadInput

instance Data.Aeson.FromJSON TestHttpPayloadInput where
    parseJSON = Data.Aeson.withObject "TestHttpPayloadInput" $ \v -> TestHttpPayloadInput
        Data.Functor.<$> (v Data.Aeson..: "payload")
        Control.Applicative.<*> (v Data.Aeson..: "identifier")
        Control.Applicative.<*> (v Data.Aeson..:? "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..:? "prefixHeaders")
    



data TestHttpPayloadInputBuilderState = TestHttpPayloadInputBuilderState {
    payloadBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    identifierBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
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

type TestHttpPayloadInputBuilder = Control.Monad.State.Strict.State TestHttpPayloadInputBuilderState

setPayload :: Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpPayloadInputBuilder ()
setPayload value =
   Control.Monad.State.Strict.modify (\s -> (s { payloadBuilderState = Data.Maybe.Just value }))

setIdentifier :: Data.Int.Int32 -> TestHttpPayloadInputBuilder ()
setIdentifier value =
   Control.Monad.State.Strict.modify (\s -> (s { identifierBuilderState = Data.Maybe.Just value }))

setStringheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpPayloadInputBuilder ()
setStringheader value =
   Control.Monad.State.Strict.modify (\s -> (s { stringHeaderBuilderState = value }))

setPrefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpPayloadInputBuilder ()
setPrefixheaders value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixHeadersBuilderState = value }))

build :: TestHttpPayloadInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Com.Example.Utility.IntoRequestBuilder TestHttpPayloadInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Com.Example.Utility.setPath [
            "payload",
            Com.Example.Utility.serializeElement (identifier self)
            ]
        
        Com.Example.Utility.serHeaderMap "x-prefix-" (prefixHeaders self)
        Com.Example.Utility.serHeader "x-header-string" (stringHeader self)
        Com.Example.Utility.serBody "application/json" (payload self)

