{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpDocumentOutput (
    setMessage,
    build,
    TestHttpDocumentOutputBuilder,
    TestHttpDocumentOutput,
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

data TestHttpDocumentOutput = TestHttpDocumentOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpDocumentOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    


instance Data.Aeson.FromJSON TestHttpDocumentOutput where
    parseJSON = Data.Aeson.withObject "TestHttpDocumentOutput" $ \v -> TestHttpDocumentOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestHttpDocumentOutputBuilderState = TestHttpDocumentOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpDocumentOutputBuilderState
defaultBuilderState = TestHttpDocumentOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

newtype TestHttpDocumentOutputBuilder a = TestHttpDocumentOutputBuilder {
    runTestHttpDocumentOutputBuilder :: TestHttpDocumentOutputBuilderState -> (TestHttpDocumentOutputBuilderState, a)
}

instance Data.Functor.Functor TestHttpDocumentOutputBuilder where
    fmap f (TestHttpDocumentOutputBuilder g) =
        TestHttpDocumentOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpDocumentOutputBuilder where
    pure a = TestHttpDocumentOutputBuilder (\s -> (s, a))
    (TestHttpDocumentOutputBuilder f) <*> (TestHttpDocumentOutputBuilder g) = TestHttpDocumentOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpDocumentOutputBuilder where
    (TestHttpDocumentOutputBuilder f) >>= g = TestHttpDocumentOutputBuilder (\s ->
        let (s', a) = f s
            (TestHttpDocumentOutputBuilder h) = g a
        in h s')

setMessage :: Data.Text.Text -> TestHttpDocumentOutputBuilder ()
setMessage value =
   TestHttpDocumentOutputBuilder (\s -> (s { messageBuilderState = Data.Maybe.Just value }, ()))

build :: TestHttpDocumentOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpDocumentOutput
build builder = do
    let (st, _) = runTestHttpDocumentOutputBuilder builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpDocumentOutput.TestHttpDocumentOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestHttpDocumentOutput { 
        message = message'
    })


