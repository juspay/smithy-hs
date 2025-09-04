module Com.Example.Model.TestQueryInput (
    setPage,
    setCoffeetype,
    setEnabled,
    setTags,
    setUtc,
    setPosixts,
    setTime,
    setMapqueryparams,
    build,
    TestQueryInputBuilder,
    TestQueryInput,
    page,
    coffeeType,
    enabled,
    tags,
    utc,
    posixTs,
    time,
    mapQueryParams
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
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified Data.Time.Clock.POSIX
import qualified GHC.Generics
import qualified GHC.Show
import qualified Network.HTTP.Date
import qualified Network.HTTP.Types.Method

data TestQueryInput = TestQueryInput {
    page :: Data.Maybe.Maybe Data.Int.Int32,
    coffeeType :: Data.Maybe.Maybe Data.Text.Text,
    enabled :: Data.Maybe.Maybe Bool,
    tags :: Data.Maybe.Maybe ([] Data.Text.Text),
    utc :: Data.Maybe.Maybe Data.Time.UTCTime,
    posixTs :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
    time :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    mapQueryParams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Show.Show,
  Data.Eq.Eq,
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestQueryInput where
    toJSON a = Data.Aeson.object [
        "page" Data.Aeson..= page a,
        "coffeeType" Data.Aeson..= coffeeType a,
        "enabled" Data.Aeson..= enabled a,
        "tags" Data.Aeson..= tags a,
        "utc" Data.Aeson..= utc a,
        "posixTs" Data.Aeson..= posixTs a,
        "time" Data.Aeson..= ((time a) Data.Functor.<&> (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate)),
        "mapQueryParams" Data.Aeson..= mapQueryParams a
        ]
    

instance Com.Example.Utility.SerializeBody TestQueryInput

instance Data.Aeson.FromJSON TestQueryInput where
    parseJSON = Data.Aeson.withObject "TestQueryInput" $ \v -> TestQueryInput
        Data.Functor.<$> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "coffeeType")
        Control.Applicative.<*> (v Data.Aeson..: "enabled")
        Control.Applicative.<*> (v Data.Aeson..: "tags")
        Control.Applicative.<*> (v Data.Aeson..: "utc")
        Control.Applicative.<*> (v Data.Aeson..: "posixTs")
        Control.Applicative.<*> (v Data.Aeson..: "time"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.TestQueryInput.TestQueryInput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..: "mapQueryParams")
    



data TestQueryInputBuilderState = TestQueryInputBuilderState {
    pageBuilderState :: Data.Maybe.Maybe Data.Int.Int32,
    coffeeTypeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    enabledBuilderState :: Data.Maybe.Maybe Bool,
    tagsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    utcBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    posixTsBuilderState :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
    timeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    mapQueryParamsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: TestQueryInputBuilderState
defaultBuilderState = TestQueryInputBuilderState {
    pageBuilderState = Data.Maybe.Nothing,
    coffeeTypeBuilderState = Data.Maybe.Nothing,
    enabledBuilderState = Data.Maybe.Nothing,
    tagsBuilderState = Data.Maybe.Nothing,
    utcBuilderState = Data.Maybe.Nothing,
    posixTsBuilderState = Data.Maybe.Nothing,
    timeBuilderState = Data.Maybe.Nothing,
    mapQueryParamsBuilderState = Data.Maybe.Nothing
}

type TestQueryInputBuilder = Control.Monad.State.Strict.State TestQueryInputBuilderState

setPage :: Data.Maybe.Maybe Data.Int.Int32 -> TestQueryInputBuilder ()
setPage value =
   Control.Monad.State.Strict.modify (\s -> (s { pageBuilderState = value }))

setCoffeetype :: Data.Maybe.Maybe Data.Text.Text -> TestQueryInputBuilder ()
setCoffeetype value =
   Control.Monad.State.Strict.modify (\s -> (s { coffeeTypeBuilderState = value }))

setEnabled :: Data.Maybe.Maybe Bool -> TestQueryInputBuilder ()
setEnabled value =
   Control.Monad.State.Strict.modify (\s -> (s { enabledBuilderState = value }))

setTags :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestQueryInputBuilder ()
setTags value =
   Control.Monad.State.Strict.modify (\s -> (s { tagsBuilderState = value }))

setUtc :: Data.Maybe.Maybe Data.Time.UTCTime -> TestQueryInputBuilder ()
setUtc value =
   Control.Monad.State.Strict.modify (\s -> (s { utcBuilderState = value }))

setPosixts :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime -> TestQueryInputBuilder ()
setPosixts value =
   Control.Monad.State.Strict.modify (\s -> (s { posixTsBuilderState = value }))

setTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> TestQueryInputBuilder ()
setTime value =
   Control.Monad.State.Strict.modify (\s -> (s { timeBuilderState = value }))

setMapqueryparams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestQueryInputBuilder ()
setMapqueryparams value =
   Control.Monad.State.Strict.modify (\s -> (s { mapQueryParamsBuilderState = value }))

build :: TestQueryInputBuilder () -> Data.Either.Either Data.Text.Text TestQueryInput
build builder = do
    let (_, st) = Control.Monad.State.Strict.runState builder defaultBuilderState
    page' <- Data.Either.Right (pageBuilderState st)
    coffeeType' <- Data.Either.Right (coffeeTypeBuilderState st)
    enabled' <- Data.Either.Right (enabledBuilderState st)
    tags' <- Data.Either.Right (tagsBuilderState st)
    utc' <- Data.Either.Right (utcBuilderState st)
    posixTs' <- Data.Either.Right (posixTsBuilderState st)
    time' <- Data.Either.Right (timeBuilderState st)
    mapQueryParams' <- Data.Either.Right (mapQueryParamsBuilderState st)
    Data.Either.Right (TestQueryInput { 
        page = page',
        coffeeType = coffeeType',
        enabled = enabled',
        tags = tags',
        utc = utc',
        posixTs = posixTs',
        time = time',
        mapQueryParams = mapQueryParams'
    })


instance Com.Example.Utility.IntoRequestBuilder TestQueryInput where
    intoRequestBuilder self = do
        Com.Example.Utility.setMethod Network.HTTP.Types.Method.methodGet
        Com.Example.Utility.setPath [
            "query_params"
            ]
        Com.Example.Utility.serQuery "query_literal" ("some_query_literal_value" :: Data.Text.Text)
        Com.Example.Utility.serQueryMap (mapQueryParams self)
        Com.Example.Utility.serQuery "type" (coffeeType self)
        Com.Example.Utility.serQuery "utc" (utc self)
        Com.Example.Utility.serQuery "page" (page self)
        Com.Example.Utility.serQuery "posix-ts" (posixTs self)
        Com.Example.Utility.serQuery "time" (time self)
        Com.Example.Utility.serQuery "enabled" (enabled self)
        Com.Example.Utility.serQuery "tags" (tags self)
        
        

