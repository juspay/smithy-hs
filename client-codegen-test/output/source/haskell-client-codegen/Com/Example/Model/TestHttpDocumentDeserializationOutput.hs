module Com.Example.Model.TestHttpDocumentDeserializationOutput (
    setOutputheader,
    setOutputheaderint,
    setOutputheaderbool,
    setOutputheaderlist,
    setOutputprefixheaders,
    setItem,
    setCustomization,
    setTime,
    build,
    TestHttpDocumentDeserializationOutputBuilder,
    TestHttpDocumentDeserializationOutput,
    outputHeader,
    outputHeaderInt,
    outputHeaderBool,
    outputHeaderList,
    outputPrefixHeaders,
    item,
    customization,
    time
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
import qualified Network.HTTP.Types

data TestHttpDocumentDeserializationOutput = TestHttpDocumentDeserializationOutput {
    outputHeader :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderInt :: Data.Maybe.Maybe Data.Int.Int32,
    outputHeaderBool :: Data.Maybe.Maybe Bool,
    outputHeaderList :: Data.Maybe.Maybe ([] Data.Text.Text),
    outputPrefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    item :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
    time :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
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
        "customization" Data.Aeson..= customization a,
        "time" Data.Aeson..= ((time a) Data.Functor.<&> (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate))
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpDocumentDeserializationOutput

instance Data.Aeson.FromJSON TestHttpDocumentDeserializationOutput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentDeserializationOutput" $ \v -> TestHttpDocumentDeserializationOutput
        Data.Functor.<$> (v Data.Aeson..: "outputHeader")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderInt")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderBool")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderList")
        Control.Applicative.<*> (v Data.Aeson..: "outputPrefixHeaders")
        Control.Applicative.<*> (v Data.Aeson..: "item")
        Control.Applicative.<*> (v Data.Aeson..: "customization")
        Control.Applicative.<*> (v Data.Aeson..: "time"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestHttpDocumentDeserializationOutput.TestHttpDocumentDeserializationOutput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
    



data TestHttpDocumentDeserializationOutputBuilderState = TestHttpDocumentDeserializationOutputBuilderState {
    outputHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderIntBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    outputHeaderBoolBuilderState :: Data.Maybe.Maybe Bool,
    outputHeaderListBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    outputPrefixHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    itemBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    customizationBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization,
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate
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
    customizationBuilderState = Data.Maybe.Nothing,
    timeBuilderState = Data.Maybe.Nothing
}

type TestHttpDocumentDeserializationOutputBuilder = Control.Monad.State.Strict.State TestHttpDocumentDeserializationOutputBuilderState

setOutputheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheader value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderBuilderState = value }))

setOutputheaderint :: Data.Maybe.Maybe Data.Int.Int32 -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheaderint value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderIntBuilderState = value }))

setOutputheaderbool :: Data.Maybe.Maybe Bool -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheaderbool value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderBoolBuilderState = value }))

setOutputheaderlist :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputheaderlist value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderListBuilderState = value }))

setOutputprefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpDocumentDeserializationOutputBuilder ()
setOutputprefixheaders value =
   Control.Monad.State.Strict.modify (\s -> (s { outputPrefixHeadersBuilderState = value }))

setItem :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpDocumentDeserializationOutputBuilder ()
setItem value =
   Control.Monad.State.Strict.modify (\s -> (s { itemBuilderState = value }))

setCustomization :: Data.Maybe.Maybe Com.Example.Model.CoffeeCustomization.CoffeeCustomization -> TestHttpDocumentDeserializationOutputBuilder ()
setCustomization value =
   Control.Monad.State.Strict.modify (\s -> (s { customizationBuilderState = value }))

setTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> TestHttpDocumentDeserializationOutputBuilder ()
setTime value =
   Control.Monad.State.Strict.modify (\s -> (s { timeBuilderState = value }))

build :: TestHttpDocumentDeserializationOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentDeserializationOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    outputHeader' <- Data.Either.Right (outputHeaderBuilderState st)
    outputHeaderInt' <- Data.Either.Right (outputHeaderIntBuilderState st)
    outputHeaderBool' <- Data.Either.Right (outputHeaderBoolBuilderState st)
    outputHeaderList' <- Data.Either.Right (outputHeaderListBuilderState st)
    outputPrefixHeaders' <- Data.Either.Right (outputPrefixHeadersBuilderState st)
    item' <- Data.Either.Right (itemBuilderState st)
    customization' <- Data.Either.Right (customizationBuilderState st)
    time' <- Data.Either.Right (timeBuilderState st)
    Data.Either.Right (TestHttpDocumentDeserializationOutput { 
        outputHeader = outputHeader',
        outputHeaderInt = outputHeaderInt',
        outputHeaderBool = outputHeaderBool',
        outputHeaderList = outputHeaderList',
        outputPrefixHeaders = outputPrefixHeaders',
        item = item',
        customization = customization',
        time = time'
    })


instance Com.Example.Utility.FromResponseParser TestHttpDocumentDeserializationOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        var0 <- Com.Example.Utility.deSerHeaderMap "x-output-prefix-"
        var1 <- Com.Example.Utility.deSerHeader "x-output-header"
        var2 <- Com.Example.Utility.deSerHeader "x-output-header-bool"
        var3 <- Com.Example.Utility.deSerHeader "x-output-header-list"
        var4 <- Com.Example.Utility.deSerHeader "x-output-header-int"
        var5 <- Com.Example.Utility.deSerField "item"
        var6 <- Com.Example.Utility.deSerField "customization"
        var7 <- Com.Example.Utility.deSerField "time"
        pure $ TestHttpDocumentDeserializationOutput {
            outputHeader = var1,
            outputHeaderInt = var4,
            outputHeaderBool = var2,
            outputHeaderList = var3,
            outputPrefixHeaders = var0,
            item = var5,
            customization = var6,
            time = var7
        }

