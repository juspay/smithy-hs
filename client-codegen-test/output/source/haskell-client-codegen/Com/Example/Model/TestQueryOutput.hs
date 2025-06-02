{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestQueryOutput (
    setMessage,
    build,
    TestQueryOutputBuilder,
    TestQueryOutput,
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

data TestQueryOutput = TestQueryOutput {
    message :: Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestQueryOutput where
    toJSON a = Data.Aeson.object [
        "message" Data.Aeson..= message a
        ]
    


instance Data.Aeson.FromJSON TestQueryOutput where
    parseJSON = Data.Aeson.withObject "TestQueryOutput" $ \v -> TestQueryOutput
        Data.Functor.<$> (v Data.Aeson..: "message")
    



data TestQueryOutputBuilderState = TestQueryOutputBuilderState {
    messageBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestQueryOutputBuilderState
defaultBuilderState = TestQueryOutputBuilderState {
    messageBuilderState = Data.Maybe.Nothing
}

newtype TestQueryOutputBuilder a = TestQueryOutputBuilder {
    runTestQueryOutputBuilder :: TestQueryOutputBuilderState -> (TestQueryOutputBuilderState, a)
}

instance Data.Functor.Functor TestQueryOutputBuilder where
    fmap f (TestQueryOutputBuilder g) =
        TestQueryOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestQueryOutputBuilder where
    pure a = TestQueryOutputBuilder (\s -> (s, a))
    (TestQueryOutputBuilder f) <*> (TestQueryOutputBuilder g) = TestQueryOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestQueryOutputBuilder where
    (TestQueryOutputBuilder f) >>= g = TestQueryOutputBuilder (\s ->
        let (s', a) = f s
            (TestQueryOutputBuilder h) = g a
        in h s')

setMessage :: Data.Text.Text -> TestQueryOutputBuilder ()
setMessage value =
   TestQueryOutputBuilder (\s -> (s { messageBuilderState = Data.Maybe.Just value }, ()))

build :: TestQueryOutputBuilder () -> Data.Either.Either Data.Text.Text TestQueryOutput
build builder = do
    let (st, _) = runTestQueryOutputBuilder builder defaultBuilderState
    message' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestQueryOutput.TestQueryOutput.message is a required property.") Data.Either.Right (messageBuilderState st)
    Data.Either.Right (TestQueryOutput { 
        message = message'
    })


