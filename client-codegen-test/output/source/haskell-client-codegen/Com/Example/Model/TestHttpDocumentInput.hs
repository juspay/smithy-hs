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
import qualified Com.Example.Utility
import qualified Control.Applicative
import qualified Control.Monad.State.Strict
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Int
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Date
import qualified Network.HTTP.Types.Method

data TestHttpDocumentInput = TestHttpDocumentInput {
    payload :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
    time :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    identifier :: Data.Int.Int32,
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
    

instance Com.Example.Utility.SerializeBody TestHttpDocumentInput

instance Data.Aeson.FromJSON TestHttpDocumentInput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentInput" $ \v -> TestHttpDocumentInput
        Data.Functor.<$> (v Data.Aeson..:? "payload")
        Control.Applicative.<*> (v Data.Aeson..:? "customization")
        Control.Applicative.<*> (v Data.Aeson..:? "time"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..: "identifier")
        Control.Applicative.<*> (v Data.Aeson..:? "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..:? "prefixHeaders")
    



data TestHttpDocumentInputBuilderState = TestHttpDocumentInputBuilderState {
    payloadBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customizationBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    identifierBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
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

type TestHttpDocumentInputBuilder = Control.Monad.State.Strict.State TestHttpDocumentInputBuilderState

setPayload :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpDocumentInputBuilder ()
setPayload value =
   Control.Monad.State.Strict.modify (\s -> (s { payloadBuilderState = value }))

setCustomization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization -> TestHttpDocumentInputBuilder ()
setCustomization value =
   Control.Monad.State.Strict.modify (\s -> (s { customizationBuilderState = value }))

setTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> TestHttpDocumentInputBuilder ()
setTime value =
   Control.Monad.State.Strict.modify (\s -> (s { timeBuilderState = value }))

setIdentifier :: Data.Int.Int32 -> TestHttpDocumentInputBuilder ()
setIdentifier value =
   Control.Monad.State.Strict.modify (\s -> (s { identifierBuilderState = Data.Maybe.Just value }))

setStringheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpDocumentInputBuilder ()
setStringheader value =
   Control.Monad.State.Strict.modify (\s -> (s { stringHeaderBuilderState = value }))

setPrefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpDocumentInputBuilder ()
setPrefixheaders value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixHeadersBuilderState = value }))

build :: TestHttpDocumentInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
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


instance Com.Example.Utility.IntoRequestBuilder TestHttpDocumentInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodPost
        Com.Example.Utility.setPath [
            "document",
            Com.Example.Utility.serializeElement (identifier self)
            ]
        
        Com.Example.Utility.serHeaderMap "x-prefix-" (prefixHeaders self)
        Com.Example.Utility.serHeader "x-header-string" (stringHeader self)
        Com.Example.Utility.serField "payload" (payload self)
        Com.Example.Utility.serField "customization" (customization self)
        Com.Example.Utility.serField "time" (time self)

