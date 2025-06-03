{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpDocumentDeserializationOutput (
    setOutputheader,
    setOutputheaderint,
    setOutputheaderbool,
    setOutputheaderlist,
    setOutputprefixheaders,
    setItem,
    setCustomization,
    build,
    TestHttpDocumentDeserializationOutputBuilder,
    TestHttpDocumentDeserializationOutput,
    outputHeader,
    outputHeaderInt,
    outputHeaderBool,
    outputHeaderList,
    outputPrefixHeaders,
    item,
    customization
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

data TestHttpDocumentDeserializationOutput = TestHttpDocumentDeserializationOutput {
    outputHeader :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderInt :: Data.Maybe.Maybe Integer,
    outputHeaderBool :: Data.Maybe.Maybe Bool,
    outputHeaderList :: Data.Maybe.Maybe ([] Data.Text.Text),
    outputPrefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    item :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpDocumentDeserializationOutput where
    toJSON a = Data.Aeson.object [
        "outputHeader" Data.Aeson..= outputHeader a,
        "outputHeaderInt" Data.Aeson..= outputHeaderInt a,
        "outputHeaderBool" Data.Aeson..= outputHeaderBool a,
        "outputHeaderList" Data.Aeson..= outputHeaderList a,
        "outputPrefixHeaders" Data.Aeson..= outputPrefixHeaders a,
        "item" Data.Aeson..= item a,
        "customization" Data.Aeson..= customization a
        ]
    


instance Data.Aeson.FromJSON TestHttpDocumentDeserializationOutput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentDeserializationOutput" $ \v -> TestHttpDocumentDeserializationOutput
        Data.Functor.<$> (v Data.Aeson..: "outputHeader")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderInt")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderBool")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderList")
        Control.Applicative.<*> (v Data.Aeson..: "outputPrefixHeaders")
        Control.Applicative.<*> (v Data.Aeson..: "item")
        Control.Applicative.<*> (v Data.Aeson..: "customization")
    



data TestHttpDocumentDeserializationOutputBuilderState = TestHttpDocumentDeserializationOutputBuilderState {
    outputHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderIntBuilderState :: Data.Maybe.Maybe Integer,
    outputHeaderBoolBuilderState :: Data.Maybe.Maybe Bool,
    outputHeaderListBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    outputPrefixHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    itemBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customizationBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpDocumentDeserializationOutputBuilderState
defaultBuilderState = TestHttpDocumentDeserializationOutputBuilderState {
    outputHeaderBuilderState = Data.Maybe.Nothing,
    outputHeaderIntBuilderState = Data.Maybe.Nothing,
    outputHeaderBoolBuilderState = Data.Maybe.Nothing,
    outputHeaderListBuilderState = Data.Maybe.Nothing,
    outputPrefixHeadersBuilderState = Data.Maybe.Nothing,
    itemBuilderState = Data.Maybe.Nothing,
    customizationBuilderState = Data.Maybe.Nothing
}

newtype TestHttpDocumentDeserializationOutputBuilder a = TestHttpDocumentDeserializationOutputBuilder {
    runTestHttpDocumentDeserializationOutputBuilder :: TestHttpDocumentDeserializationOutputBuilderState -> (TestHttpDocumentDeserializationOutputBuilderState, a)
}

instance Data.Functor.Functor TestHttpDocumentDeserializationOutputBuilder where
    fmap f (TestHttpDocumentDeserializationOutputBuilder g) =
        TestHttpDocumentDeserializationOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpDocumentDeserializationOutputBuilder where
    pure a = TestHttpDocumentDeserializationOutputBuilder (\s -> (s, a))
    (TestHttpDocumentDeserializationOutputBuilder f) <*> (TestHttpDocumentDeserializationOutputBuilder g) = TestHttpDocumentDeserializationOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpDocumentDeserializationOutputBuilder where
    (TestHttpDocumentDeserializationOutputBuilder f) >>= g = TestHttpDocumentDeserializationOutputBuilder (\s ->
        let (s', a) = f s
            (TestHttpDocumentDeserializationOutputBuilder h) = g a
        in h s')

setOutputheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheader value =
   TestHttpDocumentDeserializationOutputBuilder (\s -> (s { outputHeaderBuilderState = value }, ()))

setOutputheaderint :: Data.Maybe.Maybe Integer -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheaderint value =
   TestHttpDocumentDeserializationOutputBuilder (\s -> (s { outputHeaderIntBuilderState = value }, ()))

setOutputheaderbool :: Data.Maybe.Maybe Bool -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheaderbool value =
   TestHttpDocumentDeserializationOutputBuilder (\s -> (s { outputHeaderBoolBuilderState = value }, ()))

setOutputheaderlist :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheaderlist value =
   TestHttpDocumentDeserializationOutputBuilder (\s -> (s { outputHeaderListBuilderState = value }, ()))

setOutputprefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputprefixheaders value =
   TestHttpDocumentDeserializationOutputBuilder (\s -> (s { outputPrefixHeadersBuilderState = value }, ()))

setItem :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpDocumentDeserializationOutputBuilder ()
setItem value =
   TestHttpDocumentDeserializationOutputBuilder (\s -> (s { itemBuilderState = value }, ()))

setCustomization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization -> TestHttpDocumentDeserializationOutputBuilder ()
setCustomization value =
   TestHttpDocumentDeserializationOutputBuilder (\s -> (s { customizationBuilderState = value }, ()))

build :: TestHttpDocumentDeserializationOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentDeserializationOutput
build builder = do
    let (st, _) = runTestHttpDocumentDeserializationOutputBuilder builder defaultBuilderState
    outputHeader' <- Data.Either.Right (outputHeaderBuilderState st)
    outputHeaderInt' <- Data.Either.Right (outputHeaderIntBuilderState st)
    outputHeaderBool' <- Data.Either.Right (outputHeaderBoolBuilderState st)
    outputHeaderList' <- Data.Either.Right (outputHeaderListBuilderState st)
    outputPrefixHeaders' <- Data.Either.Right (outputPrefixHeadersBuilderState st)
    item' <- Data.Either.Right (itemBuilderState st)
    customization' <- Data.Either.Right (customizationBuilderState st)
    Data.Either.Right (TestHttpDocumentDeserializationOutput { 
        outputHeader = outputHeader',
        outputHeaderInt = outputHeaderInt',
        outputHeaderBool = outputHeaderBool',
        outputHeaderList = outputHeaderList',
        outputPrefixHeaders = outputPrefixHeaders',
        item = item',
        customization = customization'
    })


