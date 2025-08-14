module Com.Example.Model.TestHttpPayloadDeserializationOutput (
    setOutputheader,
    setOutputheaderint,
    setOutputheaderbool,
    setTime,
    setUtcheader,
    setPosixheader,
    setOutputheaderlist,
    setOutputprefixheaders,
    setItem,
    build,
    TestHttpPayloadDeserializationOutputBuilder,
    TestHttpPayloadDeserializationOutput,
    outputHeader,
    outputHeaderInt,
    outputHeaderBool,
    time,
    utcHeader,
    posixHeader,
    outputHeaderList,
    outputPrefixHeaders,
    item
) where
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
import qualified Data.Time
import qualified Data.Time.Clock.POSIX
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Date
import qualified Network.HTTP.Types

data TestHttpPayloadDeserializationOutput = TestHttpPayloadDeserializationOutput {
    outputHeader :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderInt :: Data.Maybe.Maybe Data.Int.Int32,
    outputHeaderBool :: Data.Maybe.Maybe Bool,
    time :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    utcHeader :: Data.Maybe.Maybe Data.Time.UTCTime,
    posixHeader :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
    outputHeaderList :: Data.Maybe.Maybe ([] Data.Text.Text),
    outputPrefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    item :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpPayloadDeserializationOutput where
    toJSON a = Data.Aeson.object [
        "outputHeader" Data.Aeson..= outputHeader a,
        "outputHeaderInt" Data.Aeson..= outputHeaderInt a,
        "outputHeaderBool" Data.Aeson..= outputHeaderBool a,
        "time" Data.Aeson..= ((time a) Data.Functor.<&> (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate)),
        "utcHeader" Data.Aeson..= utcHeader a,
        "posixHeader" Data.Aeson..= posixHeader a,
        "outputHeaderList" Data.Aeson..= outputHeaderList a,
        "outputPrefixHeaders" Data.Aeson..= outputPrefixHeaders a,
        "item" Data.Aeson..= item a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpPayloadDeserializationOutput

instance Data.Aeson.FromJSON TestHttpPayloadDeserializationOutput where
    parseJSON = Data.Aeson.withObject "TestHttpPayloadDeserializationOutput" $ \v -> TestHttpPayloadDeserializationOutput
        Data.Functor.<$> (v Data.Aeson..: "outputHeader")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderInt")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderBool")
        Control.Applicative.<*> (v Data.Aeson..: "time"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestHttpPayloadDeserializationOutput.TestHttpPayloadDeserializationOutput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..: "utcHeader")
        Control.Applicative.<*> (v Data.Aeson..: "posixHeader")
        Control.Applicative.<*> (v Data.Aeson..: "outputHeaderList")
        Control.Applicative.<*> (v Data.Aeson..: "outputPrefixHeaders")
        Control.Applicative.<*> (v Data.Aeson..: "item")
    



data TestHttpPayloadDeserializationOutputBuilderState = TestHttpPayloadDeserializationOutputBuilderState {
    outputHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    outputHeaderIntBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    outputHeaderBoolBuilderState :: Data.Maybe.Maybe Bool,
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    utcHeaderBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    posixHeaderBuilderState :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
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
    timeBuilderState = Data.Maybe.Nothing,
    utcHeaderBuilderState = Data.Maybe.Nothing,
    posixHeaderBuilderState = Data.Maybe.Nothing,
    outputHeaderListBuilderState = Data.Maybe.Nothing,
    outputPrefixHeadersBuilderState = Data.Maybe.Nothing,
    itemBuilderState = Data.Maybe.Nothing
}

type TestHttpPayloadDeserializationOutputBuilder = Control.Monad.State.Strict.State TestHttpPayloadDeserializationOutputBuilderState

setOutputheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheader value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderBuilderState = value }))

setOutputheaderint :: Data.Maybe.Maybe Data.Int.Int32 -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheaderint value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderIntBuilderState = value }))

setOutputheaderbool :: Data.Maybe.Maybe Bool -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheaderbool value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderBoolBuilderState = value }))

setTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> TestHttpPayloadDeserializationOutputBuilder ()
setTime value =
   Control.Monad.State.Strict.modify (\s -> (s { timeBuilderState = value }))

setUtcheader :: Data.Maybe.Maybe Data.Time.UTCTime -> TestHttpPayloadDeserializationOutputBuilder ()
setUtcheader value =
   Control.Monad.State.Strict.modify (\s -> (s { utcHeaderBuilderState = value }))

setPosixheader :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime -> TestHttpPayloadDeserializationOutputBuilder ()
setPosixheader value =
   Control.Monad.State.Strict.modify (\s -> (s { posixHeaderBuilderState = value }))

setOutputheaderlist :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputheaderlist value =
   Control.Monad.State.Strict.modify (\s -> (s { outputHeaderListBuilderState = value }))

setOutputprefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpPayloadDeserializationOutputBuilder ()
setOutputprefixheaders value =
   Control.Monad.State.Strict.modify (\s -> (s { outputPrefixHeadersBuilderState = value }))

setItem :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> TestHttpPayloadDeserializationOutputBuilder ()
setItem value =
   Control.Monad.State.Strict.modify (\s -> (s { itemBuilderState = value }))

build :: TestHttpPayloadDeserializationOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadDeserializationOutput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    outputHeader' <- Data.Either.Right (outputHeaderBuilderState st)
    outputHeaderInt' <- Data.Either.Right (outputHeaderIntBuilderState st)
    outputHeaderBool' <- Data.Either.Right (outputHeaderBoolBuilderState st)
    time' <- Data.Either.Right (timeBuilderState st)
    utcHeader' <- Data.Either.Right (utcHeaderBuilderState st)
    posixHeader' <- Data.Either.Right (posixHeaderBuilderState st)
    outputHeaderList' <- Data.Either.Right (outputHeaderListBuilderState st)
    outputPrefixHeaders' <- Data.Either.Right (outputPrefixHeadersBuilderState st)
    item' <- Data.Either.Right (itemBuilderState st)
    Data.Either.Right (TestHttpPayloadDeserializationOutput { 
        outputHeader = outputHeader',
        outputHeaderInt = outputHeaderInt',
        outputHeaderBool = outputHeaderBool',
        time = time',
        utcHeader = utcHeader',
        posixHeader = posixHeader',
        outputHeaderList = outputHeaderList',
        outputPrefixHeaders = outputPrefixHeaders',
        item = item'
    })


instance Com.Example.Utility.FromResponseParser TestHttpPayloadDeserializationOutput where
    expectedStatus = Network.HTTP.Types.status200
    responseParser = do
        var0 <- Com.Example.Utility.deSerHeaderMap "x-output-prefix-"
        var1 <- Com.Example.Utility.deSerHeader "x-output-header-utc"
        var2 <- Com.Example.Utility.deSerHeader "x-output-header"
        var3 <- Com.Example.Utility.deSerHeader "x-output-header-posix"
        var4 <- Com.Example.Utility.deSerHeader "x-output-header-bool"
        var5 <- Com.Example.Utility.deSerHeader "x-output-header-time"
        var6 <- Com.Example.Utility.deSerHeader "x-output-header-list"
        var7 <- Com.Example.Utility.deSerHeader "x-output-header-int"
        var8 <- Com.Example.Utility.deSerBody
        pure $ TestHttpPayloadDeserializationOutput {
            outputHeader = var2,
            outputHeaderInt = var7,
            outputHeaderBool = var4,
            time = var5,
            utcHeader = var1,
            posixHeader = var3,
            outputHeaderList = var6,
            outputPrefixHeaders = var0,
            item = var8
        }

