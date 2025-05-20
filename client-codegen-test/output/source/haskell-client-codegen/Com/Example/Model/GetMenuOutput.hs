module Com.Example.Model.GetMenuOutput (
    GetMenuOutput,
    items,
    setItems,
    build,
    GetMenuOutputBuilder
) where

import qualified Com.Example.Model.CoffeeItem
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Either
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Text

data GetMenuOutput = GetMenuOutput {
    items :: Data.Maybe.Maybe ([] (Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem))
}

data GetMenuOutputBuilderState = GetMenuOutputBuilderState {
    itemsBuilderState :: Data.Maybe.Maybe ([] (Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem))
}

defaultBuilderState :: GetMenuOutputBuilderState
defaultBuilderState = GetMenuOutputBuilderState {
    itemsBuilderState = Data.Maybe.Nothing
}

newtype GetMenuOutputBuilder a = GetMenuOutputBuilder {
    runGetMenuOutputBuilder :: GetMenuOutputBuilderState -> (GetMenuOutputBuilderState, a)
}

instance Data.Functor.Functor GetMenuOutputBuilder where
    fmap f (GetMenuOutputBuilder g) =
        GetMenuOutputBuilder (\s -> let (s', a) = g s in (s', f a))

instance Control.Applicative.Applicative GetMenuOutputBuilder where
    pure a = GetMenuOutputBuilder (\s -> (s, a))
    (GetMenuOutputBuilder f) <*> (GetMenuOutputBuilder g) = GetMenuOutputBuilder (\s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a))

instance Control.Monad.Monad GetMenuOutputBuilder where
    (GetMenuOutputBuilder f) >>= g = GetMenuOutputBuilder (\s ->
        let (s', a) = f s
            (GetMenuOutputBuilder h) = g a
        in h s')

setItems :: Data.Maybe.Maybe ([] (Data.Maybe.Maybe Com.Example.Model.CoffeeItem.CoffeeItem)) -> GetMenuOutputBuilder ()
setItems value =
   GetMenuOutputBuilder (\s -> (s { itemsBuilderState = value }, ()))

build :: GetMenuOutputBuilder () -> Data.Either.Either Data.Text.Text GetMenuOutput
build builder = do
    let (st, _) = runGetMenuOutputBuilder builder defaultBuilderState
    items' <- Data.Either.Right (itemsBuilderState st)
    Data.Either.Right (GetMenuOutput { 
        items = items'
    })


