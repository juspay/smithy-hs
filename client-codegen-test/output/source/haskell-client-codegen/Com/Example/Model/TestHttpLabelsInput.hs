module Com.Example.Model.TestHttpLabelsInput (
    setIdentifier,
    setEnabled,
    setName,
    setTime,
    setUtc,
    setPosix,
    build,
    TestHttpLabelsInputBuilder,
    TestHttpLabelsInput,
    identifier,
    enabled,
    name,
    time,
    utc,
    posix
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
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified Data.Time.Clock.POSIX
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Date
import qualified Network.HTTP.Types.Method

data TestHttpLabelsInput = TestHttpLabelsInput {
    identifier :: Data.Int.Int32,
    enabled :: Bool,
    name :: Data.Text.Text,
    time :: Network.HTTP.Date.HTTPDate,
    utc :: Data.Time.UTCTime,
    posix :: Data.Time.Clock.POSIX.POSIXTime
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
        "time" Data.Aeson..= ((time a) Data.Function.& (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate)),
        "utc" Data.Aeson..= utc a,
        "posix" Data.Aeson..= posix a
        ]
    

instance Com.Example.Utility.SerializeBody TestHttpLabelsInput

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
        Control.Applicative.<*> (v Data.Aeson..: "utc")
        Control.Applicative.<*> (v Data.Aeson..: "posix")
    



data TestHttpLabelsInputBuilderState = TestHttpLabelsInputBuilderState {
    identifierBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    enabledBuilderState :: Data.Maybe.Maybe Bool,
    nameBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    utcBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    posixBuilderState :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestHttpLabelsInputBuilderState
defaultBuilderState = TestHttpLabelsInputBuilderState {
    identifierBuilderState = Data.Maybe.Nothing,
    enabledBuilderState = Data.Maybe.Nothing,
    nameBuilderState = Data.Maybe.Nothing,
    timeBuilderState = Data.Maybe.Nothing,
    utcBuilderState = Data.Maybe.Nothing,
    posixBuilderState = Data.Maybe.Nothing
}

type TestHttpLabelsInputBuilder = Control.Monad.State.Strict.State TestHttpLabelsInputBuilderState

setIdentifier :: Data.Int.Int32 -> TestHttpLabelsInputBuilder ()
setIdentifier value =
   Control.Monad.State.Strict.modify (\s -> (s { identifierBuilderState = Data.Maybe.Just value }))

setEnabled :: Bool -> TestHttpLabelsInputBuilder ()
setEnabled value =
   Control.Monad.State.Strict.modify (\s -> (s { enabledBuilderState = Data.Maybe.Just value }))

setName :: Data.Text.Text -> TestHttpLabelsInputBuilder ()
setName value =
   Control.Monad.State.Strict.modify (\s -> (s { nameBuilderState = Data.Maybe.Just value }))

setTime :: Network.HTTP.Date.HTTPDate -> TestHttpLabelsInputBuilder ()
setTime value =
   Control.Monad.State.Strict.modify (\s -> (s { timeBuilderState = Data.Maybe.Just value }))

setUtc :: Data.Time.UTCTime -> TestHttpLabelsInputBuilder ()
setUtc value =
   Control.Monad.State.Strict.modify (\s -> (s { utcBuilderState = Data.Maybe.Just value }))

setPosix :: Data.Time.Clock.POSIX.POSIXTime -> TestHttpLabelsInputBuilder ()
setPosix value =
   Control.Monad.State.Strict.modify (\s -> (s { posixBuilderState = Data.Maybe.Just value }))

build :: TestHttpLabelsInputBuilder () -> Data.Either.Either Data.Text.Text TestHttpLabelsInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    identifier' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.identifier is a required property.") Data.Either.Right (identifierBuilderState st)
    enabled' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.enabled is a required property.") Data.Either.Right (enabledBuilderState st)
    name' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.name is a required property.") Data.Either.Right (nameBuilderState st)
    time' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.time is a required property.") Data.Either.Right (timeBuilderState st)
    utc' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.utc is a required property.") Data.Either.Right (utcBuilderState st)
    posix' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.TestHttpLabelsInput.TestHttpLabelsInput.posix is a required property.") Data.Either.Right (posixBuilderState st)
    Data.Either.Right (TestHttpLabelsInput { 
        identifier = identifier',
        enabled = enabled',
        name = name',
        time = time',
        utc = utc',
        posix = posix'
    })


instance Com.Example.Utility.IntoRequestBuilder TestHttpLabelsInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Com.Example.Utility.setPath [
            "path_params",
            Com.Example.Utility.serializeElement (identifier self),
            Com.Example.Utility.serializeElement (enabled self),
            Com.Example.Utility.serializeElement (name self),
            Com.Example.Utility.serializeElement (time self),
            Com.Example.Utility.serializeElement (utc self),
            Com.Example.Utility.serializeElement (posix self)
            ]
        
        
        

