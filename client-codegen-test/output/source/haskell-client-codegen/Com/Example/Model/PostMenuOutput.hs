{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.PostMenuOutput (
    setItems,
    setResheaders,
    setConfigTag,
    setHttpdateoptional,
    setHttpdaterequired,
    build,
    PostMenuOutputBuilder,
    PostMenuOutput,
    items,
    resHeaders,
    config_tag,
    httpDateOptional,
    httpDateRequired
) where
import qualified Com.Example.Model.CoffeeItem
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified GHC.Generics
import qualified Network.HTTP.Date

data PostMenuOutput = PostMenuOutput {
    items :: Com.Example.Model.CoffeeItem.CoffeeItem,
    resHeaders :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    config_tag :: Data.Maybe.Maybe Data.Text.Text,
    httpDateOptional :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    httpDateRequired :: Network.HTTP.Date.HTTPDate
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON PostMenuOutput where
    toJSON a = Data.Aeson.object [
        "items" Data.Aeson..= items a,
        "resHeaders" Data.Aeson..= resHeaders a,
        "config_tag" Data.Aeson..= config_tag a,
        "httpDateOptional" Data.Aeson..= ((httpDateOptional a) Data.Functor.<&> (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate)),
        "httpDateRequired" Data.Aeson..= ((httpDateRequired a) Data.Function.& (Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate))
        ]
    


instance Data.Aeson.FromJSON PostMenuOutput where
    parseJSON = Data.Aeson.withObject "PostMenuOutput" $ \v -> PostMenuOutput
        Data.Functor.<$> (v Data.Aeson..: "items")
        Control.Applicative.<*> (v Data.Aeson..: "resHeaders")
        Control.Applicative.<*> (v Data.Aeson..: "config_tag")
        Control.Applicative.<*> (v Data.Aeson..: "httpDateOptional"
             >>= \t -> t
                            Data.Functor.<&> Data.Text.Encoding.encodeUtf8
                            Data.Functor.<&> Network.HTTP.Date.parseHTTPDate
                            Data.Functor.<&> Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.PostMenuOutput.PostMenuOutput.Data.Maybe.Maybe as Network.HTTP.Date.HTTPDate") pure
                            Data.Function.& Data.Maybe.maybe (pure Data.Maybe.Nothing) pure
            
            )
        Control.Applicative.<*> (v Data.Aeson..: "httpDateRequired"
             >>= \t -> t
                            Data.Function.& Data.Text.Encoding.encodeUtf8
                            Data.Function.& Network.HTTP.Date.parseHTTPDate
                            Data.Function.& Data.Maybe.maybe (fail "Failed to parse Com.Example.Model.PostMenuOutput.PostMenuOutput.Network.HTTP.Date.HTTPDate as Network.HTTP.Date.HTTPDate") pure
            
            )
    



data PostMenuOutputBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    resHeadersBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    config_tagBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    httpDateOptionalBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    httpDateRequiredBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PostMenuOutputBuilderState
defaultBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState = Data.Maybe.Nothing,
    resHeadersBuilderState = Data.Maybe.Nothing,
    config_tagBuilderState = Data.Maybe.Nothing,
    httpDateOptionalBuilderState = Data.Maybe.Nothing,
    httpDateRequiredBuilderState = Data.Maybe.Nothing
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

setHttpdateoptional :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> PostMenuOutputBuilder ()
setHttpdateoptional value =
   PostMenuOutputBuilder (\s -> (s { httpDateOptionalBuilderState = value }, ()))

setHttpdaterequired :: Network.HTTP.Date.HTTPDate -> PostMenuOutputBuilder ()
setHttpdaterequired value =
   PostMenuOutputBuilder (\s -> (s { httpDateRequiredBuilderState = Data.Maybe.Just value }, ()))

build :: PostMenuOutputBuilder () -> Data.Either.Either Data.Text.Text PostMenuOutput
build builder = do
    let (st, _) = runPostMenuOutputBuilder builder defaultBuilderState
    items' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuOutput.PostMenuOutput.items is a required property.") Data.Either.Right (itemsBuilderState st)
    resHeaders' <- Data.Either.Right (resHeadersBuilderState st)
    config_tag' <- Data.Either.Right (config_tagBuilderState st)
    httpDateOptional' <- Data.Either.Right (httpDateOptionalBuilderState st)
    httpDateRequired' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuOutput.PostMenuOutput.httpDateRequired is a required property.") Data.Either.Right (httpDateRequiredBuilderState st)
    Data.Either.Right (PostMenuOutput { 
        items = items',
        resHeaders = resHeaders',
        config_tag = config_tag',
        httpDateOptional = httpDateOptional',
        httpDateRequired = httpDateRequired'
    })


