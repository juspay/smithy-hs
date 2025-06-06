module Com.Example.Model.TestHttpLabelsInput (
    setIdentifier,
    setEnabled,
    setName,
    setTime,
    build,
    TestHttpLabelsInputBuilder,
    TestHttpLabelsInput,
    identifier,
    enabled,
    name,
    time
) where
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Date

data TestHttpLabelsInput = TestHttpLabelsInput {
    identifier :: Integer,
    enabled :: Bool,
    name :: Data.Text.Text,
    time :: Network.HTTP.Date.HTTPDate
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestHttpLabelsInput where
    toJSON a = Data.Aeson.object [
        "identifier" Data.Aeson..= identifier a,
        "enabled" Data.Aeson..= enabled a,
        "name" Data.Aeson..= name a,
        "time" Data.Aeson..= ((time a) Data.Function.& (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate))
        ]
    


instance Data.Aeson.FromJSON TestHttpLabelsInput where
    parseJSON = Data.Aeson.withObject "TestHttpLabelsInput" $ \v -> TestHttpLabelsInput
        Data.Functor.<$> (v Data.Aeson..: "identifier")
        Control.Applicative.<*> (v Data.Aeson..: "enabled")
        Control.Applicative.<*> (v Data.Aeson..: "name")
        Control.Applicative.<*> (v Data.Aeson..: "time"
             >>= \t -> t
                            Data.Function.& Data.Text.Encoding.encodeUtf8
                            Data.Function.& Network.HTTP.Date.parseHTTPDate
                            Data.Function.& Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.Network.HTTP.Date.HTTPDate as Network.HTTP.Date.HTTPDate") pure
            
            )
    



data TestHttpLabelsInputBuilderState = TestHttpLabelsInputBuilderState {
    identifierBuilderState :: Data.Maybe.Maybe Integer,
    enabledBuilderState :: Data.Maybe.Maybe Bool,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpLabelsInputBuilderState
defaultBuilderState = TestHttpLabelsInputBuilderState {
    identifierBuilderState = Data.Maybe.Nothing,
    enabledBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    timeBuilderState = Data.Maybe.Nothing
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

setTime :: Network.HTTP.Date.HTTPDate -> TestHttpLabelsInputBuilder ()
setTime value =
   TestHttpLabelsInputBuilder (\s -> (s { timeBuilderState = Data.Maybe.Just value }, ()))

build :: TestHttpLabelsInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpLabelsInput
build builder = do
    let (st, _) = runTestHttpLabelsInputBuilder builder defaultBuilderState
    identifier' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.identifier is a required property.") Data.Either.Right (identifierBuilderState st)
    enabled' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.enabled is a required property.") Data.Either.Right (enabledBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    time' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.time is a required property.") Data.Either.Right (timeBuilderState st)
    Data.Either.Right (TestHttpLabelsInput { 
        identifier = identifier',
        enabled = enabled',
        name = name',
        time = time'
    })


