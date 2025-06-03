{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpPayloadDeserializationOutput (
    setOutputheader,
    setOutputheaderint,
    setOutputheaderbool,
    setOutputheaderlist,
    setOutputprefixheaders,
    setItem,
    build,
    TestHttpPayloadDeserializationOutputBuilder,
    TestHttpPayloadDeserializationOutput,
    outputHeader,
    outputHeaderInt,
    outputHeaderBool,
    outputHeaderList,
    outputPrefixHeaders,
    item
) where
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

data TestHttpPayloadDeserializationOutput = TestHttpPayloadDeserializationOutput {
    outputHeader :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderInt :: Data.Maybe.Maybe Integer,
    outputHeaderBool :: Data.Maybe.Maybe Bool,
    outputHeaderList :: Data.Maybe.Maybe ([] Data.Text.Text),
    outputPrefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    item :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpPayloadDeserializationOutput where
    toJSON a = Data.Aeson.object [
        "outputHeader" Data.Aeson..= outputHeader a,
        "outputHeaderInt" Data.Aeson..= outputHeaderInt a,
        "outputHeaderBool" Data.Aeson..= outputHeaderBool a,
        "outputHeaderList" Data.Aeson..= outputHeaderList a,
        "outputPrefixHeaders" Data.Aeson..= outputPrefixHeaders a,
        "item" Data.Aeson..= item a
        ]
    


instance Data.Aeson.FromJSON TestHttpPayloadDeserializationOutput where
    parseJSON = Data.Aeson.withObject "TestHttpPayloadDeserializationOutput" $ \v -> TestHttpPayloadDeserializationOutput
        Data.Functor.<$> (v Data.Aeson..: "outputHeader")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderInt")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderBool")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderList")
        Control.Applicative.<*> (v Data.Aeson..: "outputPrefixHeaders")
        Control.Applicative.<*> (v Data.Aeson..: "item")
    



data TestHttpPayloadDeserializationOutputBuilderState = TestHttpPayloadDeserializationOutputBuilderState {
    outputHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderIntBuilderState :: Data.Maybe.Maybe Integer,
    outputHeaderBoolBuilderState :: Data.Maybe.Maybe Bool,
    outputHeaderListBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    outputPrefixHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    itemBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpPayloadDeserializationOutputBuilderState
defaultBuilderState = TestHttpPayloadDeserializationOutputBuilderState {
    outputHeaderBuilderState = Data.Maybe.Nothing,
    outputHeaderIntBuilderState = Data.Maybe.Nothing,
    outputHeaderBoolBuilderState = Data.Maybe.Nothing,
    outputHeaderListBuilderState = Data.Maybe.Nothing,
    outputPrefixHeadersBuilderState = Data.Maybe.Nothing,
    itemBuilderState = Data.Maybe.Nothing
}

newtype TestHttpPayloadDeserializationOutputBuilder a = TestHttpPayloadDeserializationOutputBuilder {
    runTestHttpPayloadDeserializationOutputBuilder :: TestHttpPayloadDeserializationOutputBuilderState -> (TestHttpPayloadDeserializationOutputBuilderState, a)
}

instance Data.Functor.Functor TestHttpPayloadDeserializationOutputBuilder where
    fmap f (TestHttpPayloadDeserializationOutputBuilder g) =
        TestHttpPayloadDeserializationOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpPayloadDeserializationOutputBuilder where
    pure a = TestHttpPayloadDeserializationOutputBuilder (\s -> (s, a))
    (TestHttpPayloadDeserializationOutputBuilder f) <*> (TestHttpPayloadDeserializationOutputBuilder g) = TestHttpPayloadDeserializationOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpPayloadDeserializationOutputBuilder where
    (TestHttpPayloadDeserializationOutputBuilder f) >>= g = TestHttpPayloadDeserializationOutputBuilder (\s ->
        let (s', a) = f s
            (TestHttpPayloadDeserializationOutputBuilder h) = g a
        in h s')

setOutputheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheader value =
   TestHttpPayloadDeserializationOutputBuilder (\s -> (s { outputHeaderBuilderState = value }, ()))

setOutputheaderint :: Data.Maybe.Maybe Integer -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheaderint value =
   TestHttpPayloadDeserializationOutputBuilder (\s -> (s { outputHeaderIntBuilderState = value }, ()))

setOutputheaderbool :: Data.Maybe.Maybe Bool -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheaderbool value =
   TestHttpPayloadDeserializationOutputBuilder (\s -> (s { outputHeaderBoolBuilderState = value }, ()))

setOutputheaderlist :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheaderlist value =
   TestHttpPayloadDeserializationOutputBuilder (\s -> (s { outputHeaderListBuilderState = value }, ()))

setOutputprefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputprefixheaders value =
   TestHttpPayloadDeserializationOutputBuilder (\s -> (s { outputPrefixHeadersBuilderState = value }, ()))

setItem :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpPayloadDeserializationOutputBuilder ()
setItem value =
   TestHttpPayloadDeserializationOutputBuilder (\s -> (s { itemBuilderState = value }, ()))

build :: TestHttpPayloadDeserializationOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadDeserializationOutput
build builder = do
    let (st, _) = runTestHttpPayloadDeserializationOutputBuilder builder defaultBuilderState
    outputHeader' <- Data.Either.Right (outputHeaderBuilderState st)
    outputHeaderInt' <- Data.Either.Right (outputHeaderIntBuilderState st)
    outputHeaderBool' <- Data.Either.Right (outputHeaderBoolBuilderState st)
    outputHeaderList' <- Data.Either.Right (outputHeaderListBuilderState st)
    outputPrefixHeaders' <- Data.Either.Right (outputPrefixHeadersBuilderState st)
    item' <- Data.Either.Right (itemBuilderState st)
    Data.Either.Right (TestHttpPayloadDeserializationOutput { 
        outputHeader = outputHeader',
        outputHeaderInt = outputHeaderInt',
        outputHeaderBool = outputHeaderBool',
        outputHeaderList = outputHeaderList',
        outputPrefixHeaders = outputPrefixHeaders',
        item = item'
    })


