{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.PostMenuInput (
    setItem,
    setUnionitem,
    setListqueryparams,
    setPage,
    setExperimenttype,
    setStatus,
    setSome,
    setTags,
    setVersions,
    build,
    PostMenuInputBuilder,
    PostMenuInput,
    item,
    unionItem,
    listQueryParams,
    page,
    experimentType,
    status,
    some,
    tags,
    versions
) where
import qualified Com.Example.Model.CoffeeItem
import qualified Com.Example.Model.SomeUnion
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics

data PostMenuInput = PostMenuInput {
    item :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    unionItem :: Data.Maybe.Maybe Com.Example.Model.SomeUnion.SomeUnion,
    listQueryParams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text)),
    page :: Data.Maybe.Maybe Integer,
    experimentType :: Data.Text.Text,
    status :: [] Data.Text.Text,
    some :: Data.Text.Text,
    tags :: Data.Maybe.Maybe Data.Text.Text,
    versions :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON PostMenuInput where
    toJSON a = Data.Aeson.object
        [ "item" Data.Aeson..= item a
        , "unionItem" Data.Aeson..= unionItem a
        , "listQueryParams" Data.Aeson..= listQueryParams a
        , "page" Data.Aeson..= page a
        , "experimentType" Data.Aeson..= experimentType a
        , "status" Data.Aeson..= status a
        , "some" Data.Aeson..= some a
        , "tags" Data.Aeson..= tags a
        , "versions" Data.Aeson..= versions a
        ]
    


instance Data.Aeson.FromJSON PostMenuInput where
    parseJSON = Data.Aeson.withObject "PostMenuInput" $ \v -> PostMenuInput
        Data.Functor.<$> v Data.Aeson..: "item"
        Control.Applicative.<*> v Data.Aeson..: "unionItem"
        Control.Applicative.<*> v Data.Aeson..: "listQueryParams"
        Control.Applicative.<*> v Data.Aeson..: "page"
        Control.Applicative.<*> v Data.Aeson..: "experimentType"
        Control.Applicative.<*> v Data.Aeson..: "status"
        Control.Applicative.<*> v Data.Aeson..: "some"
        Control.Applicative.<*> v Data.Aeson..: "tags"
        Control.Applicative.<*> v Data.Aeson..: "versions"
    



data PostMenuInputBuilderState = PostMenuInputBuilderState {
    itemBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    unionItemBuilderState :: Data.Maybe.Maybe Com.Example.Model.SomeUnion.SomeUnion,
    listQueryParamsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text)),
    pageBuilderState :: Data.Maybe.Maybe Integer,
    experimentTypeBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    statusBuilderState :: Data.Maybe.Maybe ([] Data.Text.Text),
    someBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    tagsBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    versionsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text)
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PostMenuInputBuilderState
defaultBuilderState = PostMenuInputBuilderState {
    itemBuilderState = Data.Maybe.Nothing,
    unionItemBuilderState = Data.Maybe.Nothing,
    listQueryParamsBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    experimentTypeBuilderState = Data.Maybe.Nothing,
    statusBuilderState = Data.Maybe.Nothing,
    someBuilderState = Data.Maybe.Nothing,
    tagsBuilderState = Data.Maybe.Nothing,
    versionsBuilderState = Data.Maybe.Nothing
}

newtype PostMenuInputBuilder a = PostMenuInputBuilder {
    runPostMenuInputBuilder :: PostMenuInputBuilderState -> (PostMenuInputBuilderState, a)
}

instance Data.Functor.Functor PostMenuInputBuilder where
    fmap f (PostMenuInputBuilder g) =
        PostMenuInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative PostMenuInputBuilder where
    pure a = PostMenuInputBuilder (\s -> (s, a))
    (PostMenuInputBuilder f) <*> (PostMenuInputBuilder g) = PostMenuInputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad PostMenuInputBuilder where
    (PostMenuInputBuilder f) >>= g = PostMenuInputBuilder (\s ->
        let (s', a) = f s
            (PostMenuInputBuilder h) = g a
        in h s')

setItem :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> PostMenuInputBuilder ()
setItem value =
   PostMenuInputBuilder (\s -> (s { itemBuilderState = value }, ()))

setUnionitem :: Data.Maybe.Maybe Com.Example.Model.SomeUnion.SomeUnion -> PostMenuInputBuilder ()
setUnionitem value =
   PostMenuInputBuilder (\s -> (s { unionItemBuilderState = value }, ()))

setListqueryparams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text ([] Data.Text.Text)) -> PostMenuInputBuilder ()
setListqueryparams value =
   PostMenuInputBuilder (\s -> (s { listQueryParamsBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> PostMenuInputBuilder ()
setPage value =
   PostMenuInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setExperimenttype :: Data.Text.Text -> PostMenuInputBuilder ()
setExperimenttype value =
   PostMenuInputBuilder (\s -> (s { experimentTypeBuilderState = Data.Maybe.Just value }, ()))

setStatus :: [] Data.Text.Text -> PostMenuInputBuilder ()
setStatus value =
   PostMenuInputBuilder (\s -> (s { statusBuilderState = Data.Maybe.Just value }, ()))

setSome :: Data.Text.Text -> PostMenuInputBuilder ()
setSome value =
   PostMenuInputBuilder (\s -> (s { someBuilderState = Data.Maybe.Just value }, ()))

setTags :: Data.Maybe.Maybe Data.Text.Text -> PostMenuInputBuilder ()
setTags value =
   PostMenuInputBuilder (\s -> (s { tagsBuilderState = value }, ()))

setVersions :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> PostMenuInputBuilder ()
setVersions value =
   PostMenuInputBuilder (\s -> (s { versionsBuilderState = value }, ()))

build :: PostMenuInputBuilder () -> Data.Either.Either Data.Text.Text PostMenuInput
build builder = do
    let (st, _) = runPostMenuInputBuilder builder defaultBuilderState
    item' <- Data.Either.Right (itemBuilderState st)
    unionItem' <- Data.Either.Right (unionItemBuilderState st)
    listQueryParams' <- Data.Either.Right (listQueryParamsBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    experimentType' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuInput.PostMenuInput.experimentType is a required property.") Data.Either.Right (experimentTypeBuilderState st)
    status' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuInput.PostMenuInput.status is a required property.") Data.Either.Right (statusBuilderState st)
    some' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuInput.PostMenuInput.some is a required property.") Data.Either.Right (someBuilderState st)
    tags' <- Data.Either.Right (tagsBuilderState st)
    versions' <- Data.Either.Right (versionsBuilderState st)
    Data.Either.Right (PostMenuInput { 
        item = item',
        unionItem = unionItem',
        listQueryParams = listQueryParams',
        page = page',
        experimentType = experimentType',
        status = status',
        some = some',
        tags = tags',
        versions = versions'
    })


