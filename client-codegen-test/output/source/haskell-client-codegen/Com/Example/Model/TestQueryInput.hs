{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.TestQueryInput (
    setPage,
    setCoffeetype,
    setEnabled,
    setTags,
    setMapqueryparams,
    build,
    TestQueryInputBuilder,
    TestQueryInput,
    page,
    coffeeType,
    enabled,
    tags,
    mapQueryParams
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

data TestQueryInput = TestQueryInput {
    page :: Data.Maybe.Maybe Integer,
    coffeeType :: Data.Maybe.Maybe Data.Text.Text,
    enabled :: Data.Maybe.Maybe Bool,
    tags :: Data.Maybe.Maybe ([] Data.Text.Text),
    mapQueryParams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON TestQueryInput where
    toJSON a = Data.Aeson.object [
        "page" Data.Aeson..= page a,
        "coffeeType" Data.Aeson..= coffeeType a,
        "enabled" Data.Aeson..= enabled a,
        "tags" Data.Aeson..= tags a,
        "mapQueryParams" Data.Aeson..= mapQueryParams a
        ]
    


instance Data.Aeson.FromJSON TestQueryInput where
    parseJSON = Data.Aeson.withObject "TestQueryInput" $ \v -> TestQueryInput
        Data.Functor.<$> (v Data.Aeson..: "page")
        Control.Applicative.<*> (v Data.Aeson..: "coffeeType")
        Control.Applicative.<*> (v Data.Aeson..: "enabled")
        Control.Applicative.<*> (v Data.Aeson..: "tags")
        Control.Applicative.<*> (v Data.Aeson..: "mapQueryParams")
    



data TestQueryInputBuilderState = TestQueryInputBuilderState {
    pageBuilderState :: Data.Maybe.Maybe Integer,
    coffeeTypeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    enabledBuilderState :: Data.Maybe.Maybe Bool,
    tagsBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
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
    mapQueryParamsBuilderState = Data.Maybe.Nothing
}

newtype TestQueryInputBuilder a = TestQueryInputBuilder {
    runTestQueryInputBuilder :: TestQueryInputBuilderState -> (TestQueryInputBuilderState, a)
}

instance Data.Functor.Functor TestQueryInputBuilder where
    fmap f (TestQueryInputBuilder g) =
        TestQueryInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative TestQueryInputBuilder where
    pure a = TestQueryInputBuilder (\s -> (s, a))
    (TestQueryInputBuilder f) <*> (TestQueryInputBuilder g) = TestQueryInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad TestQueryInputBuilder where
    (TestQueryInputBuilder f) >>= g = TestQueryInputBuilder (\s ->
        let (s', a) = f s
            (TestQueryInputBuilder h) = g a
        in h s')

setPage :: Data.Maybe.Maybe Integer -> TestQueryInputBuilder ()
setPage value =
   TestQueryInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setCoffeetype :: Data.Maybe.Maybe Data.Text.Text -> TestQueryInputBuilder ()
setCoffeetype value =
   TestQueryInputBuilder (\s -> (s { coffeeTypeBuilderState = value }, ()))

setEnabled :: Data.Maybe.Maybe Bool -> TestQueryInputBuilder ()
setEnabled value =
   TestQueryInputBuilder (\s -> (s { enabledBuilderState = value }, ()))

setTags :: Data.Maybe.Maybe ([] Data.Text.Text) -> TestQueryInputBuilder ()
setTags value =
   TestQueryInputBuilder (\s -> (s { tagsBuilderState = value }, ()))

setMapqueryparams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> TestQueryInputBuilder ()
setMapqueryparams value =
   TestQueryInputBuilder (\s -> (s { mapQueryParamsBuilderState = value }, ()))

build :: TestQueryInputBuilder () -> Data.Either.Either Data.Text.Text TestQueryInput
build builder = do
    let (st, _) = runTestQueryInputBuilder builder defaultBuilderState
    page' <- Data.Either.Right (pageBuilderState st)
    coffeeType' <- Data.Either.Right (coffeeTypeBuilderState st)
    enabled' <- Data.Either.Right (enabledBuilderState st)
    tags' <- Data.Either.Right (tagsBuilderState st)
    mapQueryParams' <- Data.Either.Right (mapQueryParamsBuilderState st)
    Data.Either.Right (TestQueryInput { 
        page = page',
        coffeeType = coffeeType',
        enabled = enabled',
        tags = tags',
        mapQueryParams = mapQueryParams'
    })


