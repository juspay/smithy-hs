{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.PostMenuInput (
    setItem,
    setUnionitem,
    setQueryparams,
    setPage,
    setSome,
    build,
    PostMenuInputBuilder,
    PostMenuInput,
    item,
    unionItem,
    queryParams,
    page,
    some
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
    queryParams :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    page :: Data.Maybe.Maybe Integer,
    some :: Data.Text.Text
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
        ]
    



data PostMenuInputBuilderState = PostMenuInputBuilderState {
    itemBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
    unionItemBuilderState :: Data.Maybe.Maybe Com.Example.Model.SomeUnion.SomeUnion,
    queryParamsBuilderState :: Data.Maybe.Maybe (Data.Map.Map Data.Text.Text Data.Text.Text),
    pageBuilderState :: Data.Maybe.Maybe Integer,
    someBuilderState :: Data.Maybe.Maybe Data.Text.Text
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PostMenuInputBuilderState
defaultBuilderState = PostMenuInputBuilderState {
    itemBuilderState = Data.Maybe.Nothing,
    unionItemBuilderState = Data.Maybe.Nothing,
    queryParamsBuilderState = Data.Maybe.Nothing,
    pageBuilderState = Data.Maybe.Nothing,
    someBuilderState = Data.Maybe.Nothing
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

build :: PostMenuInputBuilder () -> Data.Either.Either Data.Text.Text PostMenuInput
build builder = do
    let (st, _) = runPostMenuInputBuilder builder defaultBuilderState
    item' <- Data.Either.Right (itemBuilderState st)
    unionItem' <- Data.Either.Right (unionItemBuilderState st)
    queryParams' <- Data.Either.Right (queryParamsBuilderState st)
    page' <- Data.Either.Right (pageBuilderState st)
    some' <- Data.Maybe.maybe (Data.Either.Left "Com.Example.Model.PostMenuInput.PostMenuInput.some is a required property.") Data.Either.Right (someBuilderState st)
    Data.Either.Right (PostMenuInput { 
        item = item',
        unionItem = unionItem',
        queryParams = queryParams',
        page = page',
        some = some'
    })


