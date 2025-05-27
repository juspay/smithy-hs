{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.PostMenuOutput (
    setItems,
    setResheaders,
    setConfigTag,
    build,
    PostMenuOutputBuilder,
    PostMenuOutput,
    items,
    resHeaders,
    config_tag
) where
import qualified Com.Example.Model.CoffeeItem
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics

data PostMenuOutput = PostMenuOutput {
    items :: Com.Example.Model.CoffeeItem.CoffeeItem,
    resHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    config_tag :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON PostMenuOutput where
    toJSON a = Data.Aeson.object
        [ "items" Data.Aeson..= items a
        , "resHeaders" Data.Aeson..= resHeaders a
        , "config_tag" Data.Aeson..= config_tag a
        ]
    



data PostMenuOutputBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    resHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    config_tagBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PostMenuOutputBuilderState
defaultBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState = Data.Maybe.Nothing,
    resHeadersBuilderState = Data.Maybe.Nothing,
    config_tagBuilderState = Data.Maybe.Nothing
}

newtype PostMenuOutputBuilder a = PostMenuOutputBuilder {
    runPostMenuOutputBuilder :: PostMenuOutputBuilderState -> (PostMenuOutputBuilderState, a)
}

instance Data.Functor.Functor PostMenuOutputBuilder where
    fmap f (PostMenuOutputBuilder g) =
        PostMenuOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative PostMenuOutputBuilder where
    pure a = PostMenuOutputBuilder (\s -> (s, a))
    (PostMenuOutputBuilder f) <*> (PostMenuOutputBuilder g) = PostMenuOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad PostMenuOutputBuilder where
    (PostMenuOutputBuilder f) >>= g = PostMenuOutputBuilder (\s ->
        let (s', a) = f s
            (PostMenuOutputBuilder h) = g a
        in h s')

setItems :: Com.Example.Model.CoffeeItem.CoffeeItem -> PostMenuOutputBuilder ()
setItems value =
   PostMenuOutputBuilder (\s -> (s { itemsBuilderState = Data.Maybe.Just value }, ()))

setResheaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> PostMenuOutputBuilder ()
setResheaders value =
   PostMenuOutputBuilder (\s -> (s { resHeadersBuilderState = value }, ()))

setConfigTag :: Data.Maybe.Maybe Data.Text.Text -> PostMenuOutputBuilder ()
setConfigTag value =
   PostMenuOutputBuilder (\s -> (s { config_tagBuilderState = value }, ()))

build :: PostMenuOutputBuilder () -> Data.Either.Either Data.Text.Text PostMenuOutput
build builder = do
    let (st, _) = runPostMenuOutputBuilder builder defaultBuilderState
    items' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuOutput.PostMenuOutput.items is a required property.") Data.Either.Right (itemsBuilderState st)
    resHeaders' <- Data.Either.Right (resHeadersBuilderState st)
    config_tag' <- Data.Either.Right (config_tagBuilderState st)
    Data.Either.Right (PostMenuOutput { 
        items = items',
        resHeaders = resHeaders',
        config_tag = config_tag'
    })


