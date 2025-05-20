module Com.Example.Model.PostMenuInput (
      PostMenuInput
    , item
    , setItem
    , build
    , PostMenuInputBuilder
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

data PostMenuInput = PostMenuInput {
    item :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
}

data PostMenuInputBuilderState = PostMenuInputBuilderState {
    itemBuilderState :: Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem,
}

defaultBuilderState :: PostMenuInputBuilderState
defaultBuilderState = PostMenuInputBuilderState {
    itemBuilderState = Data.Maybe.Nothing,
}

newtype PostMenuInputBuilder a = PostMenuInputBuilder {
    runPostMenuInputBuilder :: PostMenuInputBuilderState -> (PostMenuInputBuilderState, a)
}

instance Data.Functor.Functor PostMenuInputBuilder where
    fmap f (PostMenuInputBuilder g) =
        PostMenuInputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Data.Applicative.Applicative PostMenuInputBuilder where
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

setItem :: Com.Example.Model.CoffeeItem.CoffeeItem -> PostMenuInputBuilder ()
setItem value =
   PostMenuInputBuilder (\s -> (s { itemBuilderState = (value) }, ()))

build :: PostMenuInputBuilder () -> Data.Either.Either Data.Text.Text PostMenuInput
build builder = do
    item' <- Data.Either.Right (itemBuilderState builder)
    Data.Either.Right (PostMenuInput { 
        item = item',
    })


