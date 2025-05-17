module Com.Example.Model.PostMenuOutput (
      PostMenuOutput
    , items
    , setItems
    , build
    , PostMenuOutputBuilder
) where

import qualified Com.Example.Model.CoffeeItem
import qualified Control.Monad
import qualified Data.Applicative
import qualified Data.Either
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Maybe
import qualified Data.Text

data PostMenuOutput = PostMenuOutput {
    items :: Data.Maybe.Maybe ([] Com.Example.Model.CoffeeItem.CoffeeItem),
}

data PostMenuOutputBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState :: Data.Maybe.Maybe ([] Com.Example.Model.CoffeeItem.CoffeeItem),
}

defaultBuilderState :: PostMenuOutputBuilderState
defaultBuilderState = PostMenuOutputBuilderState {
    itemsBuilderState = Data.Maybe.Nothing,
}

newtype PostMenuOutputBuilder a = PostMenuOutputBuilder {
    runPostMenuOutputBuilder :: PostMenuOutputBuilderState -> (PostMenuOutputBuilderState, a)
}

instance Data.Functor.Functor PostMenuOutputBuilder where
    fmap f (PostMenuOutputBuilder g) =
        PostMenuOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Data.Applicative.Applicative PostMenuOutputBuilder where
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

setItems :: [] Com.Example.Model.CoffeeItem.CoffeeItem -> PostMenuOutputBuilder ()
setItems value =
   PostMenuOutputBuilder (\s -> (s { itemsBuilderState = (value) }, ()))

build :: PostMenuOutputBuilder () -> Data.Either.Either Data.Text.Text PostMenuOutput
build builder = do
    items' <- Data.Either.Right (itemsBuilderState builder)
    Data.Either.Right (PostMenuOutput { 
        items = items',
    })


