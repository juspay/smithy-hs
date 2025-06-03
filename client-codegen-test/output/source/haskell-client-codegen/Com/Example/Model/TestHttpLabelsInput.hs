{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestHttpLabelsInput (
    setIdentifier,
    setEnabled,
    setName,
    build,
    TestHttpLabelsInputBuilder,
    TestHttpLabelsInput,
    identifier,
    enabled,
    name
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

data TestHttpLabelsInput = TestHttpLabelsInput {
    identifier :: Integer,
    enabled :: Bool,
    name :: Data.Text.Text
} deriving (
  GHC.Show.Show,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpLabelsInput where
    toJSON a = Data.Aeson.object [
        "identifier" Data.Aeson..= identifier a,
        "enabled" Data.Aeson..= enabled a,
        "name" Data.Aeson..= name a
        ]
    


instance Data.Aeson.FromJSON TestHttpLabelsInput where
    parseJSON = Data.Aeson.withObject "TestHttpLabelsInput" $ \v -> TestHttpLabelsInput
        Data.Functor.<$> (v Data.Aeson..: "identifier")
        Control.Applicative.<*> (v Data.Aeson..: "enabled")
        Control.Applicative.<*> (v Data.Aeson..: "name")
    



data TestHttpLabelsInputBuilderState = TestHttpLabelsInputBuilderState {
    identifierBuilderState :: Data.Maybe.Maybe Integer,
    enabledBuilderState :: Data.Maybe.Maybe Bool,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpLabelsInputBuilderState
defaultBuilderState = TestHttpLabelsInputBuilderState {
    identifierBuilderState = Data.Maybe.Nothing,
    enabledBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing
}

newtype TestHttpLabelsInputBuilder a = TestHttpLabelsInputBuilder {
    runTestHttpLabelsInputBuilder :: TestHttpLabelsInputBuilderState -> (TestHttpLabelsInputBuilderState, a)
}

instance Data.Functor.Functor TestHttpLabelsInputBuilder where
    fmap f (TestHttpLabelsInputBuilder g) =
        TestHttpLabelsInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestHttpLabelsInputBuilder where
    pure a = TestHttpLabelsInputBuilder (\s -> (s, a))
    (TestHttpLabelsInputBuilder f) <*> (TestHttpLabelsInputBuilder g) = TestHttpLabelsInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestHttpLabelsInputBuilder where
    (TestHttpLabelsInputBuilder f) >>= g = TestHttpLabelsInputBuilder (\s ->
        let (s', a) = f s
            (TestHttpLabelsInputBuilder h) = g a
        in h s')

setIdentifier :: Integer -> TestHttpLabelsInputBuilder ()
setIdentifier value =
   TestHttpLabelsInputBuilder (\s -> (s { identifierBuilderState = Data.Maybe.Just value }, ()))

setEnabled :: Bool -> TestHttpLabelsInputBuilder ()
setEnabled value =
   TestHttpLabelsInputBuilder (\s -> (s { enabledBuilderState = Data.Maybe.Just value }, ()))

setName :: Data.Text.Text -> TestHttpLabelsInputBuilder ()
setName value =
   TestHttpLabelsInputBuilder (\s -> (s { nameBuilderState = Data.Maybe.Just value }, ()))

build :: TestHttpLabelsInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpLabelsInput
build builder = do
    let (st, _) = runTestHttpLabelsInputBuilder builder defaultBuilderState
    identifier' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.identifier is a required property.") Data.Either.Right (identifierBuilderState st)
    enabled' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.enabled is a required property.") Data.Either.Right (enabledBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    Data.Either.Right (TestHttpLabelsInput { 
        identifier = identifier',
        enabled = enabled',
        name = name'
    })


