{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpHeadersInput (
    setIntheader,
    setStringheader,
    setBoolheader,
    setListheader,
    setPrefixheaders,
    build,
    TestHttpHeadersInputBuilder,
    TestHttpHeadersInput,
    intHeader,
    stringHeader,
    boolHeader,
    listHeader,
    prefixHeaders
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics
import qualified GHC.Show

data TestHttpHeadersInput = TestHttpHeadersInput {
    intHeader :: Data.Maybe.Maybe Integer,
    stringHeader :: Data.Maybe.Maybe Data.Text.Text,
    boolHeader :: Data.Maybe.Maybe Bool,
    listHeader :: Data.Maybe.Maybe ([] Data.Text.Text),
    prefixHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Show.Show,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpHeadersInput where
    toJSON a = Data.Aeson.object [
        "intHeader" Data.Aeson..= intHeader a,
        "stringHeader" Data.Aeson..= stringHeader a,
        "boolHeader" Data.Aeson..= boolHeader a,
        "listHeader" Data.Aeson..= listHeader a,
        "prefixHeaders" Data.Aeson..= prefixHeaders a
        ]
    


instance Data.Aeson.FromJSON TestHttpHeadersInput where
    parseJSON = Data.Aeson.withObject "TestHttpHeadersInput" $ \v -> TestHttpHeadersInput
        Data.Functor.<$> (v Data.Aeson..: "intHeader")
        Control.Applicative.<*> (v Data.Aeson..: "stringHeader")
        Control.Applicative.<*> (v Data.Aeson..: "boolHeader")
        Control.Applicative.<*> (v Data.Aeson..: "listHeader")
        Control.Applicative.<*> (v Data.Aeson..: "prefixHeaders")
    



data TestHttpHeadersInputBuilderState = TestHttpHeadersInputBuilderState {
    intHeaderBuilderState :: Data.Maybe.Maybe Integer,
    stringHeaderBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    boolHeaderBuilderState :: Data.Maybe.Maybe Bool,
    listHeaderBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
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
    prefixHeaders' <- Data.Either.Right (prefixHeadersBuilderState st)
    Data.Either.Right (TestHttpHeadersInput { 
        intHeader = intHeader',
        stringHeader = stringHeader',
        boolHeader = boolHeader',
        listHeader = listHeader',
        prefixHeaders = prefixHeaders'
    })


