{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpDocumentInput (
    setPayload,
    setCustomization,
    setIdentifier,
    setStringheader,
    setPrefixheaders,
    build,
    TestHttpDocumentInputBuilder,
    TestHttpDocumentInput,
    payload,
    customization,
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
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics

data TestHttpDocumentInput = TestHttpDocumentInput {
    payload :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
    identifier :: Integer,
    stringHeader :: Data.Maybe.Maybe Data.Text.Text,
    prefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpDocumentInput where
    toJSON a = Data.Aeson.object [
        "payload" Data.Aeson..= payload a,
        "customization" Data.Aeson..= customization a,
        "identifier" Data.Aeson..= identifier a,
        "stringHeader" Data.Aeson..= stringHeader a,
        "prefixHeaders" Data.Aeson..= prefixHeaders a
        ]
    


instance Data.Aeson.FromJSON TestHttpDocumentInput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentInput" $ \v -> TestHttpDocumentInput
        Data.Functor.<$> (v Data.Aeson..: "payload")
        Control.Applicative.<*> (v Data.Aeson..: "customization")
        Control.Applicative.<*> (v Data.Aeson..: "identifier")
        Control.Applicative.<*> (v Data.Aeson..: "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..: "prefixHeaders")
    



data TestHttpDocumentInputBuilderState = TestHttpDocumentInputBuilderState {
    payloadBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customizationBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
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
    identifier' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpDocumentInput.TestHttpDocumentInput.identifier is a required property.") Data.Either.Right (identifierBuilderState st)
    stringHeader' <- Data.Either.Right (stringHeaderBuilderState st)
    prefixHeaders' <- Data.Either.Right (prefixHeadersBuilderState st)
    Data.Either.Right (TestHttpDocumentInput { 
        payload = payload',
        customization = customization',
        identifier = identifier',
        stringHeader = stringHeader',
        prefixHeaders = prefixHeaders'
    })


