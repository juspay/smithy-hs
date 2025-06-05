{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Com.Example.Model.GetMenuOutput (
    setItems,
    build,
    GetMenuOutputBuilder,
    GetMenuOutput,
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

data GetMenuOutput = GetMenuOutput {
    items :: Data.Maybe.Maybe ([] Com.Example.Model.CoffeeItem.CoffeeItem)
} deriving (
  GHC.Generics.Generic
  )

instance Data.Aeson.ToJSON GetMenuOutput where
    toJSON a = Data.Aeson.object [
        "items" Data.Aeson..= items a
        ]
    


instance Data.Aeson.FromJSON GetMenuOutput where
    parseJSON = Data.Aeson.withObject "GetMenuOutput" $ \v -> GetMenuOutput
        Data.Functor.<$> (v Data.Aeson..: "items")
    



data GetMenuOutputBuilderState = GetMenuOutputBuilderState {
    itemsBuilderState :: Data.Maybe.Maybe ([] Com.Example.Model.CoffeeItem.CoffeeItem)
} deriving (
  GHC.Generics.Generic
  )

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

setItems :: Data.Maybe.Maybe ([] Com.Example.Model.CoffeeItem.CoffeeItem) -> GetMenuOutputBuilder ()
setItems value =
   GetMenuOutputBuilder (\s -> (s { itemsBuilderState = value }, ()))

build :: GetMenuOutputBuilder () -> Data.Either.Either Data.Text.Text GetMenuOutput
build builder = do
    let (st, _) = runGetMenuOutputBuilder builder defaultBuilderState
    items' <- Data.Either.Right (itemsBuilderState st)
    Data.Either.Right (GetMenuOutput { 
        items = items'
    })


