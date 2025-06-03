{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpPayloadOutput (
    setMessage,
    build,
    TestHttpPayloadOutputBuilder,
    TestHttpPayloadOutput,
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

data TestHttpPayloadOutput = TestHttpPayloadOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpPayloadOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    


instance Data.Aeson.FromJSON TestHttpPayloadOutput where
    parseJSON = Data.Aeson.withObject "TestHttpPayloadOutput" $ \v -> TestHttpPayloadOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestHttpPayloadOutputBuilderState = TestHttpPayloadOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpPayloadOutputBuilderState
defaultBuilderState = TestHttpPayloadOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

newtype TestHttpPayloadOutputBuilder a = TestHttpPayloadOutputBuilder {
    runTestHttpPayloadOutputBuilder :: TestHttpPayloadOutputBuilderState -> (TestHttpPayloadOutputBuilderState, a)
}

instance Data.Functor.Functor TestHttpPayloadOutputBuilder where
    fmap f (TestHttpPayloadOutputBuilder g) =
        TestHttpPayloadOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpPayloadOutputBuilder where
    pure a = TestHttpPayloadOutputBuilder (\s -> (s, a))
    (TestHttpPayloadOutputBuilder f) <*> (TestHttpPayloadOutputBuilder g) = TestHttpPayloadOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpPayloadOutputBuilder where
    (TestHttpPayloadOutputBuilder f) >>= g = TestHttpPayloadOutputBuilder (\s ->
        let (s', a) = f s
            (TestHttpPayloadOutputBuilder h) = g a
        in h s')

setMessage :: Data.Text.Text -> TestHttpPayloadOutputBuilder ()
setMessage value =
   TestHttpPayloadOutputBuilder (\s -> (s { messageBuilderState = Data.Maybe.Just value }, ()))

build :: TestHttpPayloadOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpPayloadOutput
build builder = do
    let (st, _) = runTestHttpPayloadOutputBuilder builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpPayloadOutput.TestHttpPayloadOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestHttpPayloadOutput { 
        message = message'
    })


