module Com.Example.Model.TestHttpHeadersInput (
    setIntheader,
    setStringheader,
    setBoolheader,
    setListheader,
    setTime,
    setUtc,
    setPosix,
    setPrefixheaders,
    build,
    TestHttpHeadersInputBuilder,
    TestHttpHeadersInput,
    intHeader,
    stringHeader,
    boolHeader,
    listHeader,
    time,
    utc,
    posix,
    prefixHeaders
) where
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
import qualified Network.HTTP.Types.Method

data TestHttpHeadersInput = TestHttpHeadersInput {
    intHeader :: Data.Maybe.Maybe Data.Int.Int32,
    stringHeader :: Data.Maybe.Maybe Data.Text.Text,
    boolHeader :: Data.Maybe.Maybe Bool,
    listHeader :: Data.Maybe.Maybe ([] Data.Text.Text),
    time :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    utc :: Data.Maybe.Maybe Data.Time.UTCTime,
    posix :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
    prefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpHeadersInput where
    toJSON a = Data.Aeson.object [
        "intHeader" Data.Aeson..= intHeader a,
        "stringHeader" Data.Aeson..= stringHeader a,
        "boolHeader" Data.Aeson..= boolHeader a,
        "listHeader" Data.Aeson..= listHeader a,
        "time" Data.Aeson..= ((time a) Data.Functor.<&> (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate)),
        "utc" Data.Aeson..= utc a,
        "posix" Data.Aeson..= posix a,
        "prefixHeaders" Data.Aeson..= prefixHeaders a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpHeadersInput

instance Data.Aeson.FromJSON TestHttpHeadersInput where
    parseJSON = Data.Aeson.withObject "TestHttpHeadersInput" $ \v -> TestHttpHeadersInput
        Data.Functor.<$> (v Data.Aeson..:? "intHeader")
        Control.Applicative.<*> (v Data.Aeson..:? "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..:? "boolHeader")
        Control.Applicative.<*> (v Data.Aeson..:? "listHeader")
        Control.Applicative.<*> (v Data.Aeson..:? "time"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestHttpHeadersInput.TestHttpHeadersInput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..:? "utc")
        Control.Applicative.<*> (v Data.Aeson..:? "posix")
        Control.Applicative.<*> (v Data.Aeson..:? "prefixHeaders")
    



data TestHttpHeadersInputBuilderState = TestHttpHeadersInputBuilderState {
    intHeaderBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    stringHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    boolHeaderBuilderState :: Data.Maybe.Maybe Bool,
    listHeaderBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    utcBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    posixBuilderState :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
    prefixHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpHeadersInputBuilderState
defaultBuilderState = TestHttpHeadersInputBuilderState {
    intHeaderBuilderState = Data.Maybe.Nothing,
    stringHeaderBuilderState = Data.Maybe.Nothing,
    boolHeaderBuilderState = Data.Maybe.Nothing,
    listHeaderBuilderState = Data.Maybe.Nothing,
    timeBuilderState = Data.Maybe.Nothing,
    utcBuilderState = Data.Maybe.Nothing,
    posixBuilderState = Data.Maybe.Nothing,
    prefixHeadersBuilderState = Data.Maybe.Nothing
}

type TestHttpHeadersInputBuilder = Control.Monad.State.Strict.State TestHttpHeadersInputBuilderState

setIntheader :: Data.Maybe.Maybe Data.Int.Int32 -> TestHttpHeadersInputBuilder ()
setIntheader value =
   Control.Monad.State.Strict.modify (\s -> (s { intHeaderBuilderState = value }))

setStringheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpHeadersInputBuilder ()
setStringheader value =
   Control.Monad.State.Strict.modify (\s -> (s { stringHeaderBuilderState = value }))

setBoolheader :: Data.Maybe.Maybe Bool -> TestHttpHeadersInputBuilder ()
setBoolheader value =
   Control.Monad.State.Strict.modify (\s -> (s { boolHeaderBuilderState = value }))

setListheader :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestHttpHeadersInputBuilder ()
setListheader value =
   Control.Monad.State.Strict.modify (\s -> (s { listHeaderBuilderState = value }))

setTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> TestHttpHeadersInputBuilder ()
setTime value =
   Control.Monad.State.Strict.modify (\s -> (s { timeBuilderState = value }))

setUtc :: Data.Maybe.Maybe Data.Time.UTCTime -> TestHttpHeadersInputBuilder ()
setUtc value =
   Control.Monad.State.Strict.modify (\s -> (s { utcBuilderState = value }))

setPosix :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime -> TestHttpHeadersInputBuilder ()
setPosix value =
   Control.Monad.State.Strict.modify (\s -> (s { posixBuilderState = value }))

setPrefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpHeadersInputBuilder ()
setPrefixheaders value =
   Control.Monad.State.Strict.modify (\s -> (s { prefixHeadersBuilderState = value }))

build :: TestHttpHeadersInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpHeadersInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    intHeader' <- Data.Either.Right (intHeaderBuilderState st)
    stringHeader' <- Data.Either.Right (stringHeaderBuilderState st)
    boolHeader' <- Data.Either.Right (boolHeaderBuilderState st)
    listHeader' <- Data.Either.Right (listHeaderBuilderState st)
    time' <- Data.Either.Right (timeBuilderState st)
    utc' <- Data.Either.Right (utcBuilderState st)
    posix' <- Data.Either.Right (posixBuilderState st)
    prefixHeaders' <- Data.Either.Right (prefixHeadersBuilderState st)
    Data.Either.Right (TestHttpHeadersInput { 
        intHeader = intHeader',
        stringHeader = stringHeader',
        boolHeader = boolHeader',
        listHeader = listHeader',
        time = time',
        utc = utc',
        posix = posix',
        prefixHeaders = prefixHeaders'
    })


instance Com.Example.Utility.IntoRequestBuilder TestHttpHeadersInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Com.Example.Utility.setPath [
            "headers"
            ]
        
        Com.Example.Utility.serHeaderMap "x-prefix-" (prefixHeaders self)
        Com.Example.Utility.serHeader "x-header-bool" (boolHeader self)
        Com.Example.Utility.serHeader "x-header-int" (intHeader self)
        Com.Example.Utility.serHeader "x-header-utc" (utc self)
        Com.Example.Utility.serHeader "x-header-list" (listHeader self)
        Com.Example.Utility.serHeader "x-header-time" (time self)
        Com.Example.Utility.serHeader "x-header-posix" (posix self)
        Com.Example.Utility.serHeader "x-header-string" (stringHeader self)
        

