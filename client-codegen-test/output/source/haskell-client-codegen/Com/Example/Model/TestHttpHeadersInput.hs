module Com.Example.Model.TestHttpHeadersInput (
    setIntheader,
    setStringheader,
    setBoolheader,
    setListheader,
    setTime,
    setPrefixheaders,
    build,
    TestHttpHeadersInputBuilder,
    TestHttpHeadersInput,
    intHeader,
    stringHeader,
    boolHeader,
    listHeader,
    time,
    prefixHeaders
) where
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

data TestHttpHeadersInput = TestHttpHeadersInput {
    intHeader :: Data.Maybe.Maybe Integer,
    stringHeader :: Data.Maybe.Maybe Data.Text.Text,
    boolHeader :: Data.Maybe.Maybe Bool,
    listHeader :: Data.Maybe.Maybe ([] Data.Text.Text),
    time :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
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
        "prefixHeaders" Data.Aeson..= prefixHeaders a
        ]
    


instance Data.Aeson.FromJSON TestHttpHeadersInput where
    parseJSON = Data.Aeson.withObject "TestHttpHeadersInput" $ \v -> TestHttpHeadersInput
        Data.Functor.<$> (v Data.Aeson..: "intHeader")
        Control.Applicative.<*> (v Data.Aeson..: "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..: "boolHeader")
        Control.Applicative.<*> (v Data.Aeson..: "listHeader")
        Control.Applicative.<*> (v Data.Aeson..: "time"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestHttpHeadersInput.TestHttpHeadersInput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..: "prefixHeaders")
    



data TestHttpHeadersInputBuilderState = TestHttpHeadersInputBuilderState {
    intHeaderBuilderState :: Data.Maybe.Maybe Integer,
    stringHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    boolHeaderBuilderState :: Data.Maybe.Maybe Bool,
    listHeaderBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
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
    prefixHeadersBuilderState = Data.Maybe.Nothing
}

newtype TestHttpHeadersInputBuilder a = TestHttpHeadersInputBuilder {
    runTestHttpHeadersInputBuilder :: TestHttpHeadersInputBuilderState -> (TestHttpHeadersInputBuilderState, a)
}

instance Data.Functor.Functor TestHttpHeadersInputBuilder where
    fmap f (TestHttpHeadersInputBuilder g) =
        TestHttpHeadersInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpHeadersInputBuilder where
    pure a = TestHttpHeadersInputBuilder (\s -> (s, a))
    (TestHttpHeadersInputBuilder f) <*> (TestHttpHeadersInputBuilder g) = TestHttpHeadersInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpHeadersInputBuilder where
    (TestHttpHeadersInputBuilder f) >>= g = TestHttpHeadersInputBuilder (\s ->
        let (s', a) = f s
            (TestHttpHeadersInputBuilder h) = g a
        in h s')

setIntheader :: Data.Maybe.Maybe Integer -> TestHttpHeadersInputBuilder ()
setIntheader value =
   TestHttpHeadersInputBuilder (\s -> (s { intHeaderBuilderState = value }, ()))

setStringheader :: Data.Maybe.Maybe Data.Text.Text -> TestHttpHeadersInputBuilder ()
setStringheader value =
   TestHttpHeadersInputBuilder (\s -> (s { stringHeaderBuilderState = value }, ()))

setBoolheader :: Data.Maybe.Maybe Bool -> TestHttpHeadersInputBuilder ()
setBoolheader value =
   TestHttpHeadersInputBuilder (\s -> (s { boolHeaderBuilderState = value }, ()))

setListheader :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestHttpHeadersInputBuilder ()
setListheader value =
   TestHttpHeadersInputBuilder (\s -> (s { listHeaderBuilderState = value }, ()))

setTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> TestHttpHeadersInputBuilder ()
setTime value =
   TestHttpHeadersInputBuilder (\s -> (s { timeBuilderState = value }, ()))

setPrefixheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestHttpHeadersInputBuilder ()
setPrefixheaders value =
   TestHttpHeadersInputBuilder (\s -> (s { prefixHeadersBuilderState = value }, ()))

build :: TestHttpHeadersInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpHeadersInput
build builder = do
    let (st, _) = runTestHttpHeadersInputBuilder builder defaultBuilderState
    intHeader' <- Data.Either.Right (intHeaderBuilderState st)
    stringHeader' <- Data.Either.Right (stringHeaderBuilderState st)
    boolHeader' <- Data.Either.Right (boolHeaderBuilderState st)
    listHeader' <- Data.Either.Right (listHeaderBuilderState st)
    time' <- Data.Either.Right (timeBuilderState st)
    prefixHeaders' <- Data.Either.Right (prefixHeadersBuilderState st)
    Data.Either.Right (TestHttpHeadersInput { 
        intHeader = intHeader',
        stringHeader = stringHeader',
        boolHeader = boolHeader',
        listHeader = listHeader',
        time = time',
        prefixHeaders = prefixHeaders'
    })


