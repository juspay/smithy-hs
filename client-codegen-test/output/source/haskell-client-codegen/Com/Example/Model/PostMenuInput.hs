{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.PostMenuInput (
    setItem,
    setUnionitem,
    setQueryparams,
    setPage,
    setSome,
    setEpoch,
    setDatetime,
    setHttpdatetime,
    setDocument,
    build,
    PostMenuInputBuilder,
    PostMenuInput,
    item,
    unionItem,
    queryParams,
    page,
    some,
    epoch,
    dateTime,
    httpDateTime,
    document
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
import qualified Data.Text.Encoding
import qualified Data.Time
import qualified Data.Time.Clock.POSIX
import qualified GHC.Generics
import qualified Network.HTTP.Date

data PostMenuInput = PostMenuInput {
    item :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    unionItem :: Data.Maybe.Maybe Com.Example.Model.SomeUnion.SomeUnion,
    queryParams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    page :: Data.Maybe.Maybe Integer,
    some :: Data.Text.Text,
    epoch :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
    dateTime :: Data.Maybe.Maybe Data.Time.UTCTime,
    httpDateTime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    document :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON PostMenuInput where
    toJSON a = Data.Aeson.object
        [ "item" Data.Aeson..= item a
        , "unionItem" Data.Aeson..= unionItem a
        , "queryParams" Data.Aeson..= queryParams a
        , "page" Data.Aeson..= page a
        , "some" Data.Aeson..= some a
        , "epoch" Data.Aeson..= epoch a
        , "dateTime" Data.Aeson..= dateTime a
        , "httpDateTime" Data.Aeson..= ((Data.Text.Encoding.decodeUtf8 . Network.HTTP.Date.formatHTTPDate) Data.Functor.<$> httpDateTime a)
        , "document" Data.Aeson..= document a
        ]
    



data PostMenuInputBuilderState = PostMenuInputBuilderState {
    itemBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    unionItemBuilderState :: Data.Maybe.Maybe Com.Example.Model.SomeUnion.SomeUnion,
    queryParamsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    pageBuilderState :: Data.Maybe.Maybe Integer,
    someBuilderState :: Data.Maybe.Maybe Data.Text.Text,
    epochBuilderState :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime,
    dateTimeBuilderState :: Data.Maybe.Maybe Data.Time.UTCTime,
    httpDateTimeBuilderState :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate,
    documentBuilderState :: Data.Maybe.Maybe Data.Aeson.Value
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PostMenuInputBuilderState
defaultBuilderState = PostMenuInputBuilderState {
    itemBuilderState = Data.Maybe.Nothing,
    unionItemBuilderState = Data.Maybe.Nothing,
    queryParamsBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    someBuilderState = Data.Maybe.Nothing,
    epochBuilderState = Data.Maybe.Nothing,
    dateTimeBuilderState = Data.Maybe.Nothing,
    httpDateTimeBuilderState = Data.Maybe.Nothing,
    documentBuilderState = Data.Maybe.Nothing
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

setQueryparams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text) -> PostMenuInputBuilder ()
setQueryparams value =
   PostMenuInputBuilder (\s -> (s { queryParamsBuilderState = value }, ()))

setPage :: Data.Maybe.Maybe Integer -> PostMenuInputBuilder ()
setPage value =
   PostMenuInputBuilder (\s -> (s { pageBuilderState = value }, ()))

setSome :: Data.Text.Text -> PostMenuInputBuilder ()
setSome value =
   PostMenuInputBuilder (\s -> (s { someBuilderState = Data.Maybe.Just value }, ()))

setEpoch :: Data.Maybe.Maybe Data.Time.Clock.POSIX.POSIXTime -> PostMenuInputBuilder ()
setEpoch value =
   PostMenuInputBuilder (\s -> (s { epochBuilderState = value }, ()))

setDatetime :: Data.Maybe.Maybe Data.Time.UTCTime -> PostMenuInputBuilder ()
setDatetime value =
   PostMenuInputBuilder (\s -> (s { dateTimeBuilderState = value }, ()))

setHttpdatetime :: Data.Maybe.Maybe Network.HTTP.Date.HTTPDate -> PostMenuInputBuilder ()
setHttpdatetime value =
   PostMenuInputBuilder (\s -> (s { httpDateTimeBuilderState = value }, ()))

setDocument :: Data.Maybe.Maybe Data.Aeson.Value -> PostMenuInputBuilder ()
setDocument value =
   PostMenuInputBuilder (\s -> (s { documentBuilderState = value }, ()))

build :: PostMenuInputBuilder () -> Data.Either.Either Data.Text.Text PostMenuInput
build builder = do
    let (st, _) = runPostMenuInputBuilder builder defaultBuilderState
    item' <- Data.Either.Right (itemBuilderState st)
    unionItem' <- Data.Either.Right (unionItemBuilderState st)
    queryParams' <- Data.Either.Right (queryParamsBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    some' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuInput.PostMenuInput.some is a required property.") Data.Either.Right (someBuilderState st)
    epoch' <- Data.Either.Right (epochBuilderState st)
    dateTime' <- Data.Either.Right (dateTimeBuilderState st)
    httpDateTime' <- Data.Either.Right (httpDateTimeBuilderState st)
    document' <- Data.Either.Right (documentBuilderState st)
    Data.Either.Right (PostMenuInput { 
        item = item',
        unionItem = unionItem',
        queryParams = queryParams',
        page = page',
        some = some',
        epoch = epoch',
        dateTime = dateTime',
        httpDateTime = httpDateTime',
        document = document'
    })


