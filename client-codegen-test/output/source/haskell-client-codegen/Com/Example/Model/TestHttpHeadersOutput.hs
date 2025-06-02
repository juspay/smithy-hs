{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpHeadersOutput (
    setMessage,
    build,
    TestHttpHeadersOutputBuilder,
    TestHttpHeadersOutput,
    message
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics

data TestHttpHeadersOutput = TestHttpHeadersOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpHeadersOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    


instance Data.Aeson.FromJSON TestHttpHeadersOutput where
    parseJSON = Data.Aeson.withObject "TestHttpHeadersOutput" $ \v -> TestHttpHeadersOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestHttpHeadersOutputBuilderState = TestHttpHeadersOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpHeadersOutputBuilderState
defaultBuilderState = TestHttpHeadersOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

newtype TestHttpHeadersOutputBuilder a = TestHttpHeadersOutputBuilder {
    runTestHttpHeadersOutputBuilder :: TestHttpHeadersOutputBuilderState -> (TestHttpHeadersOutputBuilderState, a)
}

instance Data.Functor.Functor TestHttpHeadersOutputBuilder where
    fmap f (TestHttpHeadersOutputBuilder g) =
        TestHttpHeadersOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpHeadersOutputBuilder where
    pure a = TestHttpHeadersOutputBuilder (\s -> (s, a))
    (TestHttpHeadersOutputBuilder f) <*> (TestHttpHeadersOutputBuilder g) = TestHttpHeadersOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpHeadersOutputBuilder where
    (TestHttpHeadersOutputBuilder f) >>= g = TestHttpHeadersOutputBuilder (\s ->
        let (s', a) = f s
            (TestHttpHeadersOutputBuilder h) = g a
        in h s')

setMessage :: Data.Text.Text -> TestHttpHeadersOutputBuilder ()
setMessage value =
   TestHttpHeadersOutputBuilder (\s -> (s { messageBuilderState = Data.Maybe.Just value }, ()))

build :: TestHttpHeadersOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpHeadersOutput
build builder = do
    let (st, _) = runTestHttpHeadersOutputBuilder builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpHeadersOutput.TestHttpHeadersOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestHttpHeadersOutput { 
        message = message'
    })


