{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.PostMenuOutput (
    setItems,
    build,
    PostMenuOutputBuilder,
    PostMenuOutput,
    items
) where
import qualified Com.Example.Model.CoffeeItem
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Aeson
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text
import qualified GHC.Generics

data PostMenuOutput = PostMenuOutput {
    items :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON PostMenuOutput where
    toJSON a = Data.Aeson.object
        [ "items" Data.Aeson..= items a
        ]
    



data PostMenuOutputBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem
} deriving (
  GHC.Generics.Generic
  )

defaultBuilderState :: PostMenuOutputBuilderState
defaultBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState = Data.Maybe.Nothing
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

setItems :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem -> PostMenuOutputBuilder ()
setItems value =
   PostMenuOutputBuilder (\s -> (s { itemsBuilderState = value }, ()))

build :: PostMenuOutputBuilder () -> Data.Either.Either Data.Text.Text PostMenuOutput
build builder = do
    let (st, _) = runPostMenuOutputBuilder builder defaultBuilderState
    items' <- Data.Either.Right (itemsBuilderState st)
    Data.Either.Right (PostMenuOutput { 
        items = items'
    })


