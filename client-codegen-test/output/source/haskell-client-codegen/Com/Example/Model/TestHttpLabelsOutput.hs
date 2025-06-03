{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpLabelsOutput (
    setMessage,
    build,
    TestHttpLabelsOutputBuilder,
    TestHttpLabelsOutput,
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
import qualified GHC.Show

data TestHttpLabelsOutput = TestHttpLabelsOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpLabelsOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    


instance Data.Aeson.FromJSON TestHttpLabelsOutput where
    parseJSON = Data.Aeson.withObject "TestHttpLabelsOutput" $ \v -> TestHttpLabelsOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestHttpLabelsOutputBuilderState = TestHttpLabelsOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpLabelsOutputBuilderState
defaultBuilderState = TestHttpLabelsOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

newtype TestHttpLabelsOutputBuilder a = TestHttpLabelsOutputBuilder {
    runTestHttpLabelsOutputBuilder :: TestHttpLabelsOutputBuilderState -> (TestHttpLabelsOutputBuilderState, a)
}

instance Data.Functor.Functor TestHttpLabelsOutputBuilder where
    fmap f (TestHttpLabelsOutputBuilder g) =
        TestHttpLabelsOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpLabelsOutputBuilder where
    pure a = TestHttpLabelsOutputBuilder (\s -> (s, a))
    (TestHttpLabelsOutputBuilder f) <*> (TestHttpLabelsOutputBuilder g) = TestHttpLabelsOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpLabelsOutputBuilder where
    (TestHttpLabelsOutputBuilder f) >>= g = TestHttpLabelsOutputBuilder (\s ->
        let (s', a) = f s
            (TestHttpLabelsOutputBuilder h) = g a
        in h s')

setMessage :: Data.Text.Text -> TestHttpLabelsOutputBuilder ()
setMessage value =
   TestHttpLabelsOutputBuilder (\s -> (s { messageBuilderState = Data.Maybe.Just value }, ()))

build :: TestHttpLabelsOutputBuilder () -> Data.Either.Either Data.Text.Text TestHttpLabelsOutput
build builder = do
    let (st, _) = runTestHttpLabelsOutputBuilder builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsOutput.TestHttpLabelsOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestHttpLabelsOutput { 
        message = message'
    })


